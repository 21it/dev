{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Main (apply) where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Storage.Migration as Migration
import qualified RecklessTradingBot.Thread.Price as ThreadPrice

apply :: Env m => m ()
apply = do
  Migration.migrateAll
  res <- ThreadPrice.apply
  $(logTM) ErrorS
    . logStr
    $ "Terminate program with result " <> (show res :: Text)
