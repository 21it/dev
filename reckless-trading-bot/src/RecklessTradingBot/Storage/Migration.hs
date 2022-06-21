{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Storage.Migration
  ( migrateAll,
  )
where

import Database.Persist.Migration
import qualified Database.Persist.Migration.Postgres as P
  ( runMigration,
  )
import RecklessTradingBot.Import

migrateBefore :: Migration
migrateBefore = []

--
-- TODO : add all needed indexes
--
migrateAfter :: Migration
migrateAfter =
  [ 0 ~> 1 := [tradeIndexes]
  ]
  where
    tradeIndexesSql :: Text
    tradeIndexesSql =
      "CREATE INDEX IF NOT EXISTS "
        <> "trade_base_quote_idx "
        <> "ON trade (base, quote);"
    tradeIndexes =
      RawOperation "Create trade indexes" $
        lift . pure $
          [MigrateSql tradeIndexesSql []]

migrateAll :: (Storage m, KatipContext m) => m ()
migrateAll = do
  $(logTM) DebugS "Running Persistent BEFORE migrations..."
  runM migrateBefore
  $(logTM) DebugS "Running Persistent AUTO migrations..."
  runSql (runMigration migrateAuto)
  $(logTM) DebugS "Running Persistent AFTER migrations..."
  runM migrateAfter
  $(logTM) DebugS "Persistent database migrated!"
  where
    runM [] = pure ()
    runM x = do
      pool <- getSqlPool
      liftIO $ runSqlPool (P.runMigration defaultSettings x) pool
