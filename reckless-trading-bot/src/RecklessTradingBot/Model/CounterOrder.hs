{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
    bfxUpdate,
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
  Entity Order ->
  Bfx.Order 'Bfx.Buy 'Bfx.Remote ->
  TradeConf ->
  m (Entity CounterOrder)
create orderEnt bfxOrder cfg = do
  row <- liftIO $ newRow <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    exitFee = tradeConfQuoteFee cfg
    (exitGain, exitLoss, exitRate) =
      BfxMath.newCounterOrder
        (Bfx.orderAmount bfxOrder)
        (Bfx.orderRate bfxOrder)
        (orderFee $ entityVal orderEnt)
        exitFee
        $ tradeConfProfitPerOrder cfg
    newRow ct =
      CounterOrder
        { counterOrderIntRef = entityKey orderEnt,
          counterOrderExtRef = Nothing,
          counterOrderPrice = exitRate,
          counterOrderGain = exitGain,
          counterOrderLoss = exitLoss,
          counterOrderFee = exitFee,
          counterOrderStatus = OrderNew,
          counterOrderInsertedAt = ct,
          counterOrderUpdatedAt = ct
        }

bfxUpdate ::
  ( Storage m
  ) =>
  CounterOrderId ->
  Bfx.Order 'Bfx.Sell 'Bfx.Remote ->
  m ()
bfxUpdate counterId bfxCounterOrder = do
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
