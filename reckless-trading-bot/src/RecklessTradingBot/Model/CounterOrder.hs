{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.CounterOrder
  ( create,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Math as BfxMath
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import

create ::
  ( Storage m
  ) =>
  OrderId ->
  Bfx.Order 'Bfx.Buy 'Bfx.Remote ->
  Bfx.FeeRate 'Bfx.Maker 'Bfx.Quote ->
  Bfx.ProfitRate ->
  m (Entity CounterOrder)
create orderId bfxOrder fee prof = do
  row <- liftIO $ newRow <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    (exitGain, exitLoss, exitRate) =
      BfxMath.newCounterOrder
        (Bfx.orderAmount bfxOrder)
        (Bfx.orderRate bfxOrder)
        fee
        prof
    newRow ct =
      CounterOrder
        { counterOrderIntRef = orderId,
          counterOrderExtRef = Nothing,
          counterOrderPrice = exitRate,
          counterOrderGain = exitGain,
          counterOrderLoss = exitLoss,
          counterOrderFee = fee,
          counterOrderStatus = OrderNew,
          counterOrderAt = ct
        }
