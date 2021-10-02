{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Main (apply) where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Thread.Price as ThreadPrice (apply)

apply :: (Env m, KatipContext m) => m ()
apply = do
  --
  -- TODO : run migrations
  --
  res <- ThreadPrice.apply
  $(logTM) ErrorS
    . logStr
    $ "Terminate program with result " <> (show res :: Text)
