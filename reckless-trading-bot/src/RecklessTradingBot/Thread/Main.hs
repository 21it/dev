{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Main
  ( apply,
  )
where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Storage.Migration as Migration
import qualified RecklessTradingBot.Thread.CounterOrder as CounterOrder
import qualified RecklessTradingBot.Thread.Mma as Mma
import qualified RecklessTradingBot.Thread.Order as Order
import qualified RecklessTradingBot.Thread.TeleBot as TeleBot
import qualified RecklessTradingBot.Thread.TradeEnv as TradeEnv

apply :: Env m => m ()
apply = do
  Migration.migrateAll
  xs <-
    mapM
      spawnLink
      [ Mma.apply,
        TeleBot.apply,
        TradeEnv.apply,
        Order.apply,
        CounterOrder.apply
      ]
  liftIO
    . void
    $ waitAnyCancel xs
  $(logTM) ErrorS "Terminate program"
