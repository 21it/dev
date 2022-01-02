{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
    updateBfx,
    getByStatus,
    getOrdersToCounter,
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
  TradeConf ->
  Entity Order ->
  Bfx.Order 'Bfx.Buy 'Bfx.Remote ->
  m (Entity CounterOrder)
create cfg orderEnt bfxOrder = do
  row <- liftIO $ newRow <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    exitFee = tradeConfQuoteFee cfg
    (exitGain, exitLoss, exitRate) =
      case BfxMath.newCounterOrder
        (Bfx.orderAmount bfxOrder)
        (Bfx.orderRate bfxOrder)
        (orderFee $ entityVal orderEnt)
        exitFee
        $ tradeConfMinProfitPerOrder cfg of
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
          counterOrderLoss = exitLoss,
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

getByStatus ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  [OrderStatus] ->
  m [Entity CounterOrder]
getByStatus _ [] = pure []
getByStatus sym ss =
  runSql $
    P.select $
      P.from $
        \( counter
             `P.InnerJoin` order
             `P.InnerJoin` price
           ) ->
            P.distinctOn
              [ P.don $ counter P.^. CounterOrderId
              ]
              $ do
                P.on
                  ( price P.^. PriceId
                      P.==. order P.^. OrderPriceRef
                  )
                P.on
                  ( order P.^. OrderId
                      P.==. counter P.^. CounterOrderIntRef
                  )
                P.where_
                  ( ( price P.^. PriceBase
                        P.==. P.val
                          ( Bfx.currencyPairBase sym
                          )
                    )
                      P.&&. ( price P.^. PriceQuote
                                P.==. P.val
                                  ( Bfx.currencyPairQuote sym
                                  )
                            )
                      P.&&. ( counter P.^. CounterOrderStatus
                                `P.in_` P.valList ss
                            )
                  )
                P.limit 100
                P.orderBy
                  [ P.asc $
                      counter P.^. CounterOrderUpdatedAt
                  ]
                pure counter

getOrdersToCounter ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m [Entity Order]
getOrdersToCounter sym =
  runSql $
    P.select $
      P.from $
        \( counter
             `P.RightOuterJoin` order
             `P.InnerJoin` price
           ) ->
            P.distinctOn [P.don $ order P.^. OrderId] $ do
              P.on
                ( price P.^. PriceId
                    P.==. order P.^. OrderPriceRef
                )
              P.on
                ( P.just (order P.^. OrderId)
                    P.==. counter P.?. CounterOrderIntRef
                )
              P.where_
                ( ( price P.^. PriceBase
                      P.==. P.val
                        ( Bfx.currencyPairBase sym
                        )
                  )
                    P.&&. ( price P.^. PriceQuote
                              P.==. P.val
                                ( Bfx.currencyPairQuote sym
                                )
                          )
                    P.&&. ( order P.^. OrderStatus
                              P.==. P.val OrderExecuted
                          )
                    P.&&. ( ( counter P.?. CounterOrderId
                                P.==. P.val Nothing
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
              P.limit 100
              P.orderBy
                [ P.asc $
                    order P.^. OrderUpdatedAt
                ]
              pure order
