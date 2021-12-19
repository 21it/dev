{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Orphan () where

import Data.Fixed
import qualified Language.Haskell.TH.Syntax as TH (Lift)

deriving stock instance TH.Lift Pico
