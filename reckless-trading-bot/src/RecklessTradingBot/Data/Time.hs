{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Time
  ( Seconds (..),
    newSeconds,
    newSeconds',
    sleep,
  )
where

import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

newtype Seconds
  = Seconds Natural
  deriving newtype
    (Eq, Ord, Show)

newSeconds :: Integer -> Either Error Seconds
newSeconds =
  bimap
    ( \(TryFromException x _) ->
        ErrorSmartCon $
          "Seconds can not be negative but got " <> show x
    )
    Seconds
    . tryFrom

newSeconds' :: NominalDiffTime -> Either Error Seconds
newSeconds' =
  newSeconds
    . round
    . nominalDiffTimeToSeconds

sleep :: MonadIO m => Seconds -> m ()
sleep =
  liftIO
    . delay
    . (* 1000000)
    . from @Natural
    . coerce
