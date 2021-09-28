module RecklessTradingBot.Data.Time
  ( Seconds,
    newSeconds,
    sleep,
  )
where

import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

newtype Seconds
  = Seconds Integer
  deriving newtype
    (Eq, Ord, Show)

newSeconds :: Integer -> Either Error Seconds
newSeconds x
  | x >= 0 =
    Right $ Seconds x
  | otherwise =
    Left . ErrorSmartCon $
      "Seconds can not be negative but got " <> show x

sleep :: MonadIO m => Seconds -> m ()
sleep = liftIO . delay . (* 1000000) . coerce
