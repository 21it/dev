{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Storage
  ( lockByTable,
    lockByRow,
    rollback,
    commit,
  )
where

import qualified Database.Persist as P
import Database.Persist.Sql (fromSqlKey, transactionUndo)
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import.External

--
-- This fake type is here just to
-- make Haskell type system compatible
-- with weird Postgres semantics
-- for pg_advisory_xact_lock
--
data VoidSQL
  = VoidSQL

instance RawSql VoidSQL where
  rawSqlCols _ _ = (1, [])
  rawSqlColCountReason _ = ""
  rawSqlProcessRow [PersistNull] = Right VoidSQL
  rawSqlProcessRow _ = Left "Unexpected VoidSQL expr"

lockByTable :: (Enum a, MonadIO m) => a -> SqlPersistT m ()
lockByTable x =
  void
    ( rawSql
        "SELECT pg_advisory_xact_lock(?)"
        [PersistInt64 $ fromIntegral $ fromEnum x] ::
        (MonadIO m) => SqlPersistT m [VoidSQL]
    )

lockByRow ::
  ( MonadIO m,
    HasTableName a b,
    P.ToBackendKey SqlBackend a
  ) =>
  P.Key a ->
  SqlPersistT m a
lockByRow rowId = do
  void
    ( rawSql
        "SELECT pg_advisory_xact_lock(?,?)"
        [ PersistInt64 $ fromIntegral $ fromEnum $ getTableName rowId,
          PersistInt64 $ fromSqlKey rowId
        ] ::
        (MonadIO m) => SqlPersistT m [VoidSQL]
    )
  liftMaybe ("Impossible missing row " <> show rowId) =<< P.get rowId

rollback :: (KatipContext m, Show a) => a -> SqlPersistT m (Either a b)
rollback e = do
  transactionUndo
  $(logTM) DebugS $ logStr $ "Rollback " <> (show e :: Text)
  pure $ Left e

commit :: (KatipContext m, Show b) => b -> SqlPersistT m (Either a b)
commit x = do
  $(logTM) DebugS $ logStr $ "Commit " <> (show x :: Text)
  pure $ Right x

liftMaybe :: MonadIO m => String -> Maybe a -> m a
liftMaybe msg = \case
  Just x -> pure x
  Nothing -> liftIO $ fail msg
