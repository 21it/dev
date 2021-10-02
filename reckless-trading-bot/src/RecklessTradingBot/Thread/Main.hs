{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Main (apply) where

import RecklessTradingBot.Import
import qualified RecklessTradingBot.Thread.Order as ThreadOrder (loop)

apply :: (Env m, KatipContext m) => m ()
apply = do
  res <- ThreadOrder.loop
  $(logTM) ErrorS
    . logStr
    $ "Terminate program with result " <> (show res :: Text)
