{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Price (apply) where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Price as Price

apply :: Env m => m ()
apply = do
  xs <- mapM (spawnLink . loop) . toList =<< getPairs
  liftIO . void $ waitAny xs

loop :: Env m => Bfx.CurrencyPair -> m ()
loop sym = do
  sleepTillNextPrice sym
  createPrice sym
  loop sym

createPrice :: Env m => Bfx.CurrencyPair -> m ()
createPrice sym = do
  res <- runExceptT . withExceptT ErrorBfx $ do
    amt <- except $ Bfx.newMoneyAmount 2
    let getPrice x = Bfx.marketAveragePrice x amt sym
    (,) <$> getPrice Bfx.Buy <*> getPrice Bfx.Sell
  print res
  case res of
    --
    -- TODO : log error and do something
    --
    Left {} ->
      sleep $ Seconds 60
    Right (buy, sell) ->
      void $
        Price.create
          sym
          (ExchangeRate buy)
          (ExchangeRate sell)

spawnLink :: MonadUnliftIO m => m a -> m (Async a)
spawnLink x =
  withRunInIO $ \run -> do
    pid <- async $ run x
    link pid
    return pid
