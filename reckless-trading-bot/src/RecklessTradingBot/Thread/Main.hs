module RecklessTradingBot.Thread.Main (loop) where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import

loop :: Env m => m ()
loop = do
  res <- runExceptT . withExceptT ErrorBfx $ do
    sym <- except $ Bfx.newCurrencyPair "ADA" "BTC"
    amt <- except $ Bfx.newMoneyAmount 2
    Bfx.marketAveragePrice Bfx.Buy amt sym
  ct <- liftIO getCurrentTime
  putStrLn $ (show $ Bfx.toTextParam <$> res :: Text) <> " " <> show ct
  sleepPriceTtl
  loop
