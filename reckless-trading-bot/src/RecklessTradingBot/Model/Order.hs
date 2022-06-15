{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Order
  ( create,
    updateBfx,
    updateStatus,
    updateStatusSql,
    getByStatusLimit,
    getTotalInvestment,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as Psql

create ::
  ( Storage m
  ) =>
  TradeEnv ->
  Entity Trade ->
  m (Entity Order)
create cfg (Entity tradeId trade) = do
  row <- liftIO $ newOrder <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    enterPrice =
      tradeEntry trade
    enterGain =
      tradeEnvMinBuyAmt cfg
    enterLoss =
      case Bfx.roundMoney' $
        Bfx.unQuotePerBase enterPrice
          |*| Bfx.unMoney enterGain of
        Left e -> error $ show e
        Right x -> x
    newOrder ct =
      Order
        { --
          -- NOTE : some fields should be updated
          -- with real data pulled from Bitfinex
          -- after order is placed on exchange orderbook
          -- including:
          --
          --   orderExtRef
          --   orderPrice
          --   orderGain
          --   orderLoss
          --   orderStatus
          --   orderUpdatedAt
          --
          orderIntRef = tradeId,
          orderExtRef = Nothing,
          orderPrice = enterPrice,
          orderGain = enterGain,
          orderLoss = enterLoss,
          orderFee = tradeEnvBaseFee cfg,
          orderStatus = OrderNew,
          orderInsertedAt = ct,
          orderUpdatedAt = ct
        }

updateBfx ::
  ( Storage m
  ) =>
  OrderId ->
  Bfx.Order 'Bfx.Buy 'Bfx.Remote ->
  m ()
updateBfx orderId bfxOrder = do
  ct <- liftIO getCurrentTime
  runSql $
    Psql.update $ \row -> do
      Psql.set
        row
        [ OrderExtRef
            Psql.=. Psql.val
              ( Just . from $
                  Bfx.orderId bfxOrder
              ),
          OrderPrice
            Psql.=. Psql.val enterPrice,
          OrderGain
            Psql.=. Psql.val enterGain,
          OrderLoss
            Psql.=. Psql.val enterLoss,
          OrderStatus
            Psql.=. Psql.val
              ( newOrderStatus $
                  Bfx.orderStatus bfxOrder
              ),
          OrderUpdatedAt
            Psql.=. Psql.val ct
        ]
      Psql.where_
        ( row Psql.^. OrderId
            Psql.==. Psql.val orderId
        )
  where
    enterPrice =
      Bfx.orderRate bfxOrder
    enterGain =
      Bfx.orderAmount bfxOrder
    enterLoss =
      case Bfx.roundMoney' $
        Bfx.unQuotePerBase enterPrice
          |*| Bfx.unMoney enterGain of
        Left e -> error $ show e
        Right x -> x

updateStatus ::
  ( Storage m
  ) =>
  OrderStatus ->
  [OrderId] ->
  m ()
updateStatus _ [] = pure ()
updateStatus ss xs =
  runSql $
    updateStatusSql ss xs

updateStatusSql ::
  ( Storage m
  ) =>
  OrderStatus ->
  [OrderId] ->
  ReaderT SqlBackend m ()
updateStatusSql _ [] = pure ()
updateStatusSql ss xs = do
  ct <- liftIO getCurrentTime
  Psql.update $ \row -> do
    Psql.set
      row
      [ OrderStatus Psql.=. Psql.val ss,
        OrderUpdatedAt Psql.=. Psql.val ct
      ]
    Psql.where_ $
      row Psql.^. OrderId `Psql.in_` Psql.valList xs

getByStatusLimit ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  [OrderStatus] ->
  m [Entity Order]
getByStatusLimit _ [] = pure []
getByStatusLimit sym ss =
  runSql $
    Psql.select $
      Psql.from $ \(order `Psql.InnerJoin` trade) -> do
        Psql.on
          ( order Psql.^. OrderIntRef
              Psql.==. trade Psql.^. TradeId
          )
        Psql.where_
          ( ( order Psql.^. OrderStatus
                `Psql.in_` Psql.valList ss
            )
              Psql.&&. ( trade Psql.^. TradeBase
                           Psql.==. Psql.val
                             ( Bfx.currencyPairBase sym
                             )
                       )
              Psql.&&. ( trade Psql.^. TradeQuote
                           Psql.==. Psql.val
                             ( Bfx.currencyPairQuote sym
                             )
                       )
          )
        Psql.limit 10
        Psql.orderBy
          [ Psql.asc $
              order Psql.^. OrderUpdatedAt
          ]
        pure order

getTotalInvestment ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m (Bfx.Money 'Bfx.Quote 'Bfx.Buy)
getTotalInvestment sym = do
  totalInvestment <-
    runSql $
      Psql.select $
        Psql.from $ \(order `Psql.InnerJoin` trade) -> do
          Psql.on
            ( order Psql.^. OrderIntRef
                Psql.==. trade Psql.^. TradeId
            )
          Psql.where_
            ( ( order Psql.^. OrderStatus
                  `Psql.in_` Psql.valList
                    [ OrderActive,
                      OrderExecuted
                    ]
              )
                Psql.&&. ( trade Psql.^. TradeBase
                             Psql.==. Psql.val
                               ( Bfx.currencyPairBase sym
                               )
                         )
                Psql.&&. ( trade Psql.^. TradeQuote
                             Psql.==. Psql.val
                               ( Bfx.currencyPairQuote sym
                               )
                         )
            )
          pure . Psql.sum_ $
            order Psql.^. OrderLoss
  pure . fromMaybe [moneyQuoteBuy|0|] $
    Psql.unValue =<< safeHead totalInvestment
