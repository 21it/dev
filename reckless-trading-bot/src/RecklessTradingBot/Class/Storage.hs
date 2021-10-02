{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Class.Storage
  ( Storage (..),
    HasTableName (..),
  )
where

import qualified Database.Persist as P
import RecklessTradingBot.Import.External

class MonadUnliftIO m => Storage m where
  runSql :: ReaderT SqlBackend m a -> m a
  getSqlPool :: m (Pool SqlBackend)

class Enum b => HasTableName a b | a -> b where
  getTableName :: P.Key a -> b
