{-# OPTIONS_HADDOCK show-extensions #-}

module Main
  ( main,
  )
where

import RecklessTradingBot.Data.AppM (runApp)
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Thread.Main as MainThread

main :: IO ()
main =
  withEnv $
    \env -> runApp env MainThread.apply
