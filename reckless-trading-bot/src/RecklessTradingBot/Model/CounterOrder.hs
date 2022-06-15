{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
    updateBfx,
    getByStatusLimit,
    getOrdersToCounterLimit,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Math as BfxMath
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as P
import qualified RecklessTradingBot.Model.Order as Order

create ::
  ( Storage m
  ) =>
  TradeEnv ->
  Entity Trade ->
  Entity Order ->
  m (Entity CounterOrder)
create cfg tradeEnt orderEnt = do
  row <- liftIO $ newRow <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    trade = entityVal tradeEnt
    order = entityVal orderEnt
    exitFee = tradeEnvQuoteFee cfg
    exitRate = tradeTakeProfit trade
    exitLoss = orderGain order
    exitGain =
      case BfxMath.newCounterOrderSimple
        exitLoss
        exitRate
        exitFee of
        Left e -> error $ show e
        Right x -> x
    newRow ct =
      CounterOrder
        { --
          -- NOTE : some fields should be updated
          -- with real data pulled from Bitfinex
          -- after order is placed on exchange orderbook
          -- including:
          --
          --   counterOrderExtRef
          --   counterOrderPrice
          --   counterOrderGain
          --   counterOrderLoss
          --   counterOrderStatus
          --   counterOrderUpdatedAt
          --
          counterOrderIntRef = entityKey orderEnt,
          counterOrderExtRef = Nothing,
          counterOrderPrice = exitRate,
          counterOrderGain = exitGain,
          counterOrderLoss = coerce exitLoss,
          counterOrderFee = exitFee,
          counterOrderStatus = OrderNew,
          counterOrderInsertedAt = ct,
          counterOrderUpdatedAt = ct
        }

updateBfx ::
  ( Storage m
  ) =>
  Entity CounterOrder ->
  Bfx.Order 'Bfx.Sell 'Bfx.Remote ->
  m ()
updateBfx ent bfxCounter = do
  ct <- liftIO getCurrentTime
  runSql $ do
    when (counterStatus == OrderExecuted) $
      Order.updateStatusSql
        OrderCountered
        [ counterOrderIntRef $
            entityVal ent
        ]
    P.update $ \row -> do
      P.set
        row
        [ CounterOrderExtRef
            P.=. P.val
              ( Just . from $
                  Bfx.orderId bfxCounter
              ),
          CounterOrderPrice
            P.=. P.val exitPrice,
          CounterOrderGain
            P.=. P.val exitGain,
          CounterOrderLoss
            P.=. P.val exitLoss,
          CounterOrderStatus
            P.=. P.val counterStatus,
          CounterOrderUpdatedAt
            P.=. P.val ct
        ]
      P.where_
        ( row P.^. CounterOrderId
            P.==. P.val (entityKey ent)
        )
  where
    exitPrice =
      Bfx.orderRate bfxCounter
    exitLoss =
      Bfx.orderAmount bfxCounter
    exitGain =
      case Bfx.roundMoney' $
        Bfx.unQuotePerBase exitPrice
          |*| Bfx.unMoney exitLoss of
        Left e -> error $ show e
        Right x -> x
    counterStatus =
      newOrderStatus $
        Bfx.orderStatus bfxCounter

getByStatusLimit ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  [OrderStatus] ->
  m [Entity CounterOrder]
getByStatusLimit _ [] = pure []
getByStatusLimit sym ss =
  runSql $
    P.select $
      P.from $
        \( counter
             `P.InnerJoin` order
             `P.InnerJoin` trade
           ) -> do
            P.on
              ( trade P.^. TradeId
                  P.==. order P.^. OrderIntRef
              )
            P.on
              ( order P.^. OrderId
                  P.==. counter P.^. CounterOrderIntRef
              )
            P.where_
              ( ( trade P.^. TradeBase
                    P.==. P.val
                      ( Bfx.currencyPairBase sym
                      )
                )
                  P.&&. ( trade P.^. TradeQuote
                            P.==. P.val
                              ( Bfx.currencyPairQuote sym
                              )
                        )
                  P.&&. ( counter P.^. CounterOrderStatus
                            `P.in_` P.valList ss
                        )
              )
            P.limit 10
            P.orderBy
              [ P.asc $
                  counter P.^. CounterOrderUpdatedAt
              ]
            pure counter

getOrdersToCounterLimit ::
  ( Storage m
  ) =>
  m [(Entity Trade, Entity Order)]
getOrdersToCounterLimit =
  runSql $
    P.select $
      P.from $
        \( counter
             `P.RightOuterJoin` order
             `P.InnerJoin` trade
           ) -> do
            P.on
              ( trade P.^. TradeId
                  P.==. order P.^. OrderIntRef
              )
            P.on
              ( P.just (order P.^. OrderId)
                  P.==. counter P.?. CounterOrderIntRef
              )
            P.where_
              ( ( order P.^. OrderStatus
                    P.==. P.val OrderExecuted
                )
                  P.&&. ( P.isNothing
                            ( counter P.?. CounterOrderId
                            )
                            P.||. P.not_
                              ( counter P.?. CounterOrderStatus
                                  `P.in_` P.valList
                                    [ Just OrderActive,
                                      Just OrderExecuted
                                    ]
                              )
                        )
              )
            P.limit 10
            P.orderBy
              [ P.asc $
                  order P.^. OrderUpdatedAt
              ]
            pure (trade, order)
