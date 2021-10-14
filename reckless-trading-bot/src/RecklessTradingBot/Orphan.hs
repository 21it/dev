{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Orphan () where

import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import RecklessTradingBot.Import.External

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom (P.Key a) Natural
  where
  tryFrom =
    tryFrom `composeTryLhs` P.fromSqlKey
