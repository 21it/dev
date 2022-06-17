{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.CounterOrder
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.CancelOrderMulti as BfxCancel
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
  $(logTM) DebugS "Spawned"
  forever . withOperativeBfx $ do
    activeOrders <- Order.getByStatusLimit [OrderActive]
    --
    -- TODO : better rules to identity market movements
    -- to prevent unwanted trades to happen and funds
    -- being locked in a trades which will never happen.
    --
    cancelExpiredOrders activeOrders
    updateActiveOrders activeOrders
    ordersToCounter <- CounterOrder.getOrdersToCounterLimit
    $(logTM) DebugS . logStr $
      "Got orders to counter "
        <> (show ordersToCounter :: Text)
    mapM_ (uncurry counterExecutedOrder) ordersToCounter
    updateCounterOrders
      =<< CounterOrder.getByStatusLimit [OrderActive]
    sleep [seconds|5|]

cancelExpiredOrders :: (Env m) => [Entity Order] -> m ()
cancelExpiredOrders entities = do
  expiredOrders <-
    getExpiredOrders entities
  res <-
    runExceptT $
      cancelExpiredOrdersT expiredOrders
  whenLeft res $
    $(logTM) ErrorS . show

cancelExpiredOrdersT ::
  ( Env m
  ) =>
  [Entity Order] ->
  ExceptT Error m ()
cancelExpiredOrdersT [] = pure ()
cancelExpiredOrdersT entities = do
  $(logTM) DebugS $ show entities
  ids <-
    mapM
      ( \x ->
          tryJust
            ( ErrorRuntime $
                "Missing orderExtRef in "
                  <> show x
            )
            . (from <$>)
            . orderExtRef
            $ entityVal x
      )
      entities
  cids <-
    mapM
      ( \(Entity id0 x) -> do
          id1 <- tryFromT id0
          pure (id1, orderInsertedAt x)
      )
      entities
  gids <-
    mapM tryFromT $ entityKey <$> entities
  void $
    withBfxT
      Bfx.cancelOrderMulti
      ($ BfxCancel.ByOrderId $ Set.fromList ids)
  void $
    withBfxT
      Bfx.cancelOrderMulti
      ($ BfxCancel.ByOrderClientId $ Set.fromList cids)
  void $
    withBfxT
      Bfx.cancelOrderMulti
      ($ BfxCancel.ByOrderGroupId $ Set.fromList gids)

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
    Just {} -> do
      res <-
        runExceptT $
          counterExecutedT tradeEnt orderEnt
      whenLeft res $
        $(logTM) ErrorS . show

counterExecutedT ::
  ( Env m
  ) =>
  Entity Trade ->
  Entity Order ->
  ExceptT Error m ()
counterExecutedT tradeEnt orderEnt = do
  sym <-
    tryErrorT
      . Bfx.currencyPairCon (tradeBase tradeVal)
      $ tradeQuote tradeVal
  cfg <-
    getTradeEnv sym
  baseBalance <-
    withBfxT
      Bfx.spendableExchangeBalance
      ($ Bfx.currencyPairBase sym)
  when (coerce baseBalance < exitLoss) $
    throwE $
      ErrorRuntime $
        "Insufficient balance "
          <> show baseBalance
          <> " to counter "
          <> show orderEnt
  gid <-
    tryFromT $
      entityKey orderEnt
  --
  -- NOTE : Extra safety measures to prevent double-counter
  -- which should never happen anyway.
  --
  void $
    withBfxT Bfx.cancelOrderByGroupId ($ gid)
  counter <-
    lift $
      CounterOrder.create cfg tradeEnt orderEnt
  bfxCounterCid <-
    tryFromT $ entityKey counter
  currentRate <-
    withBfxT
      (const Bfx.marketAveragePrice)
      (\f -> f exitLoss sym)
  bfxCounter <-
    withBfxT
      Bfx.submitOrderMaker
      ( \submit ->
          submit
            exitLoss
            sym
            (max currentRate $ tradeTakeProfit tradeVal)
            $ ( Bfx.optsPostOnlyStopLoss
                  (tradeStopLoss tradeVal)
              )
              { Bfx.clientId = Just bfxCounterCid,
                Bfx.groupId = Just gid
              }
      )
  lift $
    CounterOrder.updateBfx
      counter
      bfxCounter
  where
    tradeVal :: Trade
    tradeVal = entityVal tradeEnt
    orderVal :: Order
    orderVal = entityVal orderEnt
    exitLoss :: Bfx.Money 'Bfx.Base 'Bfx.Sell
    exitLoss = coerce $ orderGain orderVal

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
