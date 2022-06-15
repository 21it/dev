{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.CounterOrder
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.GetOrders as BfxGetOrders
import qualified BitfinexClient.Data.SubmitOrder as Bfx
import qualified BitfinexClient.Math as BfxMath
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
  $(logTM) DebugS "Spawned"
  forever . withOperativeBfx $ do
    activeOrders <- Order.getByStatusLimit [OrderActive]
    ThreadOrder.cancelExpired activeOrders
    updateActiveOrders activeOrders
    ordersToCounter <- CounterOrder.getOrdersToCounterLimit
    $(logTM) DebugS . logStr $
      "Got orders to counter "
        <> (show ordersToCounter :: Text)
    mapM_ (uncurry counterExecutedOrder) ordersToCounter
    updateCounterOrders
      =<< CounterOrder.getByStatusLimit sym [OrderActive]
    sleep [seconds|30|]

updateActiveOrders ::
  ( Env m
  ) =>
  [Entity Order] ->
  m ()
updateActiveOrders [] = pure ()
updateActiveOrders rows = do
  res <-
    runExceptT $
      updateActiveT rows
  whenLeft res $
    $(logTM) ErrorS . show

updateActiveT ::
  ( Env m
  ) =>
  [Entity Order] ->
  ExceptT Error m ()
updateActiveT [] = pure ()
updateActiveT rows = do
  rowsWithRefs <-
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
      rows
  bfxRowMap <-
    withBfxT
      Bfx.getOrders
      ( $
          BfxGetOrders.optsIds $
            Set.fromList $
              snd <$> rowsWithRefs
      )
  lift $
    mapM_
      ( \arg@(orderEnt@(Entity orderId _), bfxId) ->
          case Map.lookup bfxId bfxRowMap of
            Nothing -> do
              $(logTM) ErrorS $
                "Missing bfx order " <> show arg
              ThreadOrder.cancelUnexpected [orderEnt]
            Just (Bfx.SomeOrder Bfx.SBuy bfxOrder) ->
              case newOrderStatus $ Bfx.orderStatus bfxOrder of
                OrderActive ->
                  Order.updateBfx orderId bfxOrder
                OrderExecuted ->
                  Order.updateBfx orderId bfxOrder
                OrderCancelled ->
                  Order.updateBfx orderId bfxOrder
                _ -> do
                  $(logTM) ErrorS $
                    "Wrong bfx status " <> show (arg, bfxOrder)
                  ThreadOrder.cancelUnexpected [orderEnt]
            Just someBfxOrder -> do
              $(logTM) ErrorS $
                "Wrong bfx order " <> show (arg, someBfxOrder)
              ThreadOrder.cancelUnexpected [orderEnt]
      )
      rowsWithRefs

counterExecutedOrder ::
  ( Env m
  ) =>
  Entity Trade ->
  Entity Order ->
  m ()
counterExecutedOrder tradeEnt orderEnt = do
  case orderExtRef $ entityVal orderEnt of
    Nothing -> do
      $(logTM) ErrorS $
        "Missing bfx ref " <> show orderEnt
      ThreadOrder.cancelUnexpected [orderEnt]
    Just ref -> do
      res <- runExceptT . counterExecutedT orderEnt $ from ref
      whenLeft res $
        $(logTM) ErrorS . show

counterExecutedT ::
  ( Env m
  ) =>
  Entity Order ->
  Bfx.OrderId ->
  ExceptT Error m ()
counterExecutedT orderEnt bfxOrderId = do
  cfg <- getTradeCfg
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
  baseBalance <-
    withBfxT
      Bfx.spendableExchangeBalance
      ($ Bfx.currencyPairBase $ tradeEnvCurrencyPair cfg)
  (_, exitLoss, _) <-
    case BfxMath.newCounterOrder
      (Bfx.orderAmount bfxOrder)
      (Bfx.orderRate bfxOrder)
      (orderFee $ entityVal orderEnt)
      (tradeEnvQuoteFee cfg)
      $ tradeEnvMinProfitPerOrder cfg of
      Left e -> throwE $ ErrorBfx e
      Right x -> pure x
  when (coerce baseBalance < exitLoss) $
    throwE $
      ErrorRuntime $
        "Insufficient balance "
          <> show baseBalance
          <> " to counter "
          <> show bfxOrder
  gid <-
    tryFromT $
      entityKey orderEnt
  --
  -- Extra safety measures to prevent double-counter
  -- which should never happen anyway.
  --
  void $
    withBfxT Bfx.cancelOrderByGroupId ($ gid)
  counter <-
    lift $
      CounterOrder.create cfg orderEnt bfxOrder
  bfxCounterCid <-
    tryFromT $ entityKey counter
  bfxCounter <-
    withBfxT
      Bfx.submitCounterOrderMaker
      ( \cont ->
          cont
            bfxOrderId
            (orderFee $ entityVal orderEnt)
            (tradeEnvQuoteFee cfg)
            (tradeEnvMinProfitPerOrder cfg)
            $ Bfx.optsPostOnly
              { Bfx.clientId = Just bfxCounterCid,
                Bfx.groupId = Just gid
              }
      )
  lift $
    CounterOrder.updateBfx
      counter
      bfxCounter

updateCounterOrders ::
  ( Env m
  ) =>
  [Entity CounterOrder] ->
  m ()
updateCounterOrders [] = pure ()
updateCounterOrders rows = do
  res <-
    runExceptT $
      updateCounterT rows
  whenLeft res $
    $(logTM) ErrorS . show

updateCounterT ::
  ( Env m
  ) =>
  [Entity CounterOrder] ->
  ExceptT Error m ()
updateCounterT [] = pure ()
updateCounterT rows = do
  rowsWithRefs <-
    mapM
      ( \row ->
          tryJust
            ( ErrorRuntime $
                "Missing counterOrderExtRef in "
                  <> show row
            )
            . ((row,) <$>)
            . (from <$>)
            . counterOrderExtRef
            $ entityVal row
      )
      rows
  bfxRowMap <-
    withBfxT
      Bfx.getOrders
      ( $
          BfxGetOrders.optsIds $
            Set.fromList $
              snd <$> rowsWithRefs
      )
  lift $
    mapM_
      ( \arg@(row, bfxId) ->
          case Map.lookup bfxId bfxRowMap of
            Nothing -> do
              $(logTM) ErrorS $
                "Missing bfx order " <> show arg
            --
            -- TODO : !!!
            --
            --ThreadCounterOrder.cancelUnexpected [orderEnt]
            Just (Bfx.SomeOrder Bfx.SSell bfxCounter) ->
              case newOrderStatus $ Bfx.orderStatus bfxCounter of
                OrderActive ->
                  CounterOrder.updateBfx row bfxCounter
                OrderExecuted ->
                  CounterOrder.updateBfx row bfxCounter
                OrderCancelled ->
                  CounterOrder.updateBfx row bfxCounter
                _ -> do
                  $(logTM) ErrorS $
                    "Wrong bfx status " <> show (arg, bfxCounter)
            --
            -- TODO : !!!
            --
            --ThreadCounterOrder.cancelUnexpected [orderEnt]
            Just someBfxOrder -> do
              $(logTM) ErrorS $
                "Wrong bfx order " <> show (arg, someBfxOrder)
                --
                -- TODO : !!!
                --
                --ThreadOrder.cancelUnexpected [orderEnt]
      )
      rowsWithRefs
