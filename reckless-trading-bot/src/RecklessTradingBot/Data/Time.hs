{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Time
  ( Seconds,
    unSeconds,
    newSeconds,
    newSeconds',
    sleep,
  )
where

import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

newtype Seconds = Seconds
  { unSeconds :: NominalDiffTime
  }
  deriving newtype
    (Eq, Ord, Show, Num)

newSeconds :: NominalDiffTime -> Either Error Seconds
newSeconds x =
  if x >= 0
    then Right $ Seconds x
    else
      Left . ErrorSmartCon $
        "Seconds can not be negative but got " <> show x

newSeconds' :: Pico -> Either Error Seconds
newSeconds' =
  newSeconds . secondsToNominalDiffTime

sleep :: MonadIO m => Seconds -> m ()
sleep =
  liftIO
    . delay
    . round
    . (* 1000000)
    . nominalDiffTimeToSeconds
    . coerce
