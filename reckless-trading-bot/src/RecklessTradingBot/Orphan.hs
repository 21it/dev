{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Orphan () where

import Data.Fixed
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import RecklessTradingBot.Import.External

deriving stock instance TH.Lift Pico

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom (P.Key a) Natural
  where
  tryFrom =
    tryFrom `composeTryLhs` P.fromSqlKey
