{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Price (apply) where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Price as Price

apply :: (Env m) => m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <- mapM (spawnLink . loop) . toList =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: (Env m) => TradingConf -> m ()
loop cfg = do
  sleepPriceTtl $ tradingConfPair cfg
  upsertPrice cfg
  loop cfg

upsertPrice :: (Env m) => TradingConf -> m ()
upsertPrice cfg = do
  res <-
    runExceptT . withExceptT ErrorBfx $
      (,) <$> getPrice @'Bfx.Buy <*> getPrice @'Bfx.Sell
  case res of
    Left e -> do
      $(logTM) ErrorS $ logStr (show e :: Text)
      sleep 60
    Right (buy, sell) -> do
      price <- Price.createUpdate sym buy sell
      putCurrPrice price
  where
    sym :: Bfx.CurrencyPair
    sym =
      tradingConfPair cfg
    getPrice ::
      forall (act :: Bfx.ExchangeAction) m.
      ( Env m,
        Bfx.ToRequestParam (Bfx.MoneyBase act)
      ) =>
      ExceptT Bfx.Error m (Bfx.QuotePerBase act)
    getPrice = do
      amt <- liftIO . readMVar $ tradingConfMinOrderAmt cfg
      Bfx.marketAveragePrice
        (coerce amt :: Bfx.MoneyBase act)
        sym
