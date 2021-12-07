{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Storage.Migration
  ( migrateAll,
  )
where

import Database.Persist.Migration
import qualified Database.Persist.Migration.Postgres as P (runMigration)
import RecklessTradingBot.Import

migrateBefore :: Migration
migrateBefore = []

--
-- TODO : add all needed indexes
--
migrateAfter :: Migration
migrateAfter =
  [ 0 ~> 1 := [priceIndexes]
  ]
  where
    priceIndexesSql :: Text
    priceIndexesSql =
      "CREATE INDEX IF NOT EXISTS "
        <> "price_base_quote_idx "
        <> "ON price (base, quote);"
    priceIndexes =
      RawOperation "Create Price indexes" $
        lift . pure $
          [MigrateSql priceIndexesSql []]

migrateAll :: (Storage m, KatipContext m) => m ()
migrateAll = do
  $(logTM) InfoS "Running Persistent BEFORE migrations..."
  runM migrateBefore
  $(logTM) InfoS "Running Persistent AUTO migrations..."
  runSql (runMigration migrateAuto)
  $(logTM) InfoS "Running Persistent AFTER migrations..."
  runM migrateAfter
  $(logTM) InfoS "Persistent database migrated!"
  where
    runM [] = pure ()
    runM x = do
      pool <- getSqlPool
      liftIO $ runSqlPool (P.runMigration defaultSettings x) pool
