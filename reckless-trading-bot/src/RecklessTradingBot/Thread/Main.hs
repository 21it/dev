{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Main
  ( apply,
  )
where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Storage.Migration as Migration
import qualified RecklessTradingBot.Thread.CounterOrder as ThreadCounterOrder
import qualified RecklessTradingBot.Thread.Order as ThreadOrder
import qualified RecklessTradingBot.Thread.Price as ThreadPrice
import qualified RecklessTradingBot.Thread.TradeConf as ThreadTradeConf

apply :: Env m => m ()
apply = do
  Migration.migrateAll
  xs <-
    mapM
      spawnLink
      [ ThreadTradeConf.apply,
        ThreadPrice.apply,
        ThreadOrder.apply,
        ThreadCounterOrder.apply
      ]
  liftIO
    . void
    $ waitAnyCancel xs
  $(logTM) ErrorS "Terminate program"
