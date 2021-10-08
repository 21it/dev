{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order (apply) where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Price as Price

apply :: Env m => m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <- mapM (spawnLink . loop) . toList =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: Env m => TradingConf -> m ()
loop cfg = do
  void rcvNextPrice
  xs <- Price.getSeq $ tradingConfPair cfg
  --
  -- TODO : !!!
  --
  print xs
  loop cfg
