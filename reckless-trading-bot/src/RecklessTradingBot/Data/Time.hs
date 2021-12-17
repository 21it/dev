{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Time
  ( Seconds,
    unSeconds,
    sleep,
  )
where

import RecklessTradingBot.Import.External

--
-- TODO : probably just use Pico
--
newtype Seconds = Seconds
  { unSeconds :: NominalDiffTime
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance TryFrom NominalDiffTime Seconds where
  tryFrom x
    | x >= 0 = Right $ Seconds x
    | otherwise = Left $ TryFromException x Nothing

instance From Seconds NominalDiffTime

instance TryFrom Pico Seconds where
  tryFrom =
    tryFrom `composeTryLhs` secondsToNominalDiffTime

instance From Seconds Pico where
  from = via @NominalDiffTime

sleep :: MonadIO m => Seconds -> m ()
sleep =
  liftIO
    . delay
    . round
    . (* 1000000)
    . nominalDiffTimeToSeconds
    . coerce
