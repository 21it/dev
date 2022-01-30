{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Time
  ( Seconds,
    seconds,
    unSeconds,
    subSeconds,
    sleep,
  )
where

import Data.Fixed
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import RecklessTradingBot.Import.External
import RecklessTradingBot.Orphan ()
import qualified Witch

newtype Seconds = Seconds
  { unSeconds :: Pico
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      FromJSON
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

seconds :: QuasiQuoter
seconds =
  mkTryQQ @Pico @Seconds

instance TryFrom Pico Seconds where
  tryFrom x
    | x >= 0 = Right $ Seconds x
    | otherwise = Left $ TryFromException x Nothing

instance From Seconds Pico

instance TryFrom NominalDiffTime Seconds where
  tryFrom =
    tryFrom `composeTryLhs` nominalDiffTimeToSeconds

instance From Seconds NominalDiffTime where
  from =
    via @Pico

subSeconds :: Seconds -> Seconds -> Seconds
subSeconds x y
  | x > y = Seconds $ from x - from y
  | otherwise = Seconds 0

sleep :: MonadIO m => Seconds -> m ()
sleep =
  liftIO
    . delay
    . round
    . (* 1000000)
    . unSeconds
