{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order (loop) where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import

loop :: Env m => m ()
loop = do
  mapM_ createPrice =<< getPairs
  sleepPriceTtl
  loop

createPrice ::
  Env m =>
  Bfx.CurrencyPair ->
  m ()
createPrice sym = do
  --
  -- TODO : !!!
  --
  res <- runExceptT . withExceptT ErrorBfx $ do
    amt <- except $ Bfx.newMoneyAmount 2
    Bfx.marketAveragePrice Bfx.Buy amt sym
  print res
