{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
    updateBfx,
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
