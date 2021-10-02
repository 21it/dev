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
    --
    -- TODO : add before/auto/after migrations
    --
    flip runApp MainThread.apply
