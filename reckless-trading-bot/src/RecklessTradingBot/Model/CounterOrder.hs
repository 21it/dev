{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
    updateBfx,
    getOrdersToCounter,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Math as BfxMath
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as P

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
  CounterOrderId ->
  Bfx.Order 'Bfx.Sell 'Bfx.Remote ->
  m ()
updateBfx counterId bfxCounterOrder = do
  ct <- liftIO getCurrentTime
  runSql $
    P.update $ \row -> do
      P.set
        row
        [ CounterOrderStatus
            P.=. P.val
              ( newOrderStatus $
                  Bfx.orderStatus bfxCounterOrder
              ),
          CounterOrderUpdatedAt
            P.=. P.val ct
        ]
      P.where_
        ( row P.^. CounterOrderId
            P.==. P.val counterId
        )

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
