{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.CounterOrder (apply) where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.GetOrders as BfxGetOrders
import qualified BitfinexClient.Data.SubmitOrder as Bfx
import qualified Data.Map as Map
import qualified Data.Set as Set
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.CounterOrder as CounterOrder
import qualified RecklessTradingBot.Model.Order as Order
import qualified RecklessTradingBot.Thread.Order as ThreadOrder

-- | This thread **must** be the only one
-- which is doing insert/update access to Order
-- model. Bot business logic is quite simple,
-- and I assume there is the only one instance
-- running. This way I can avoid lock-by-row
-- transactional complexity of postgres
-- procedures, and do simple insert/update instead.
apply :: (Env m) => m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <- mapM (spawnLink . loop) =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: (Env m) => MVar TradeConf -> m ()
loop varCfg = do
  cfg <- liftIO $ readMVar varCfg
  let sym = tradeConfCurrencyPair cfg
  updateActive . (fst <$>)
    =<< Order.getByStatus sym [OrderActive]
  mapM_ (counterExecuted cfg . fst)
    =<< Order.getByStatus sym [OrderExecuted]
  sleep [seconds|30|]
  loop varCfg

updateActive ::
  ( Env m
  ) =>
  [Entity Order] ->
  m ()
updateActive [] = pure ()
updateActive entities = do
  res <-
    runExceptT $
      updateActiveT entities
  whenLeft res $
    $(logTM) ErrorS . show

updateActiveT ::
  ( Env m
  ) =>
  [Entity Order] ->
  ExceptT Error m ()
updateActiveT [] = pure ()
updateActiveT entities = do
  ordersWithRefs <-
    mapM
      ( \x ->
          tryJust
            ( ErrorRuntime $
                "Missing orderExtRef in "
                  <> show x
            )
            . ((x,) <$>)
            . (from <$>)
            . orderExtRef
            $ entityVal x
      )
      entities
  bfxOrderMap <-
    withBfxT
      Bfx.getOrders
      ( $
          BfxGetOrders.optsIds $
            Set.fromList $
              snd <$> ordersWithRefs
      )
  ThreadOrder.cancelUnexpectedT
    . mapMaybe
      ( \(order, bfxId) ->
          maybe
            (Just order)
            ( \(Bfx.SomeOrder _ x) ->
                case newOrderStatus $ Bfx.orderStatus x of
                  OrderActive -> Nothing
                  OrderExecuted -> Nothing
                  _ -> Just order
            )
            $ Map.lookup bfxId bfxOrderMap
      )
    $ ordersWithRefs
  lift $
    Order.updateStatus OrderExecuted
      . mapMaybe
        ( \(order, bfxId) -> do
            Bfx.SomeOrder _ bfxOrder <-
              Map.lookup bfxId bfxOrderMap
            if newOrderStatus (Bfx.orderStatus bfxOrder)
              == OrderExecuted
              then Just $ entityKey order
              else Nothing
        )
      $ ordersWithRefs

counterExecuted ::
  ( Env m
  ) =>
  TradeConf ->
  Entity Order ->
  m ()
counterExecuted cfg orderEnt = do
  case orderExtRef $ entityVal orderEnt of
    Nothing ->
      ThreadOrder.cancelUnexpected [orderEnt]
    Just bfxId -> do
      res <-
        runExceptT . counterExecutedT cfg orderEnt $
          from bfxId
      whenLeft res $
        $(logTM) ErrorS . show

counterExecutedT ::
  ( Env m
  ) =>
  TradeConf ->
  Entity Order ->
  Bfx.OrderId ->
  ExceptT Error m ()
counterExecutedT cfg orderEnt@(Entity _ order) bfxOrderId = do
  bfxSomeOrder <-
    withBfxT Bfx.getOrder ($ bfxOrderId)
  bfxOrder <-
    case bfxSomeOrder of
      Bfx.SomeOrder Bfx.SBuy x ->
        pure x
      Bfx.SomeOrder Bfx.SSell _ ->
        throwE $
          ErrorBfx $
            Bfx.ErrorOrderState bfxSomeOrder
  counterId <-
    lift . (entityKey <$>) $
      CounterOrder.create orderEnt bfxOrder cfg
  bfxCounterCid <-
    tryFromT counterId
  --
  -- TODO : cancel all other counters with same
  -- order and group ids.
  --
  bfxCounterOrder <-
    withBfxT
      Bfx.submitCounterOrderMaker
      ( \cont ->
          cont
            bfxOrderId
            (orderFee order)
            (tradeConfQuoteFee cfg)
            (tradeConfMinProfitPerOrder cfg)
            $ Bfx.optsPostOnly
              { Bfx.clientId =
                  Just bfxCounterCid,
                Bfx.groupId =
                  Just $ via @Natural bfxOrderId
              }
      )
  lift $
    CounterOrder.bfxUpdate
      counterId
      bfxCounterOrder
