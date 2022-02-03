{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Price
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Price as Price

apply :: (Env m) => m ()
apply = do
  $(logTM) DebugS "Spawned"
  xs <- mapM (spawnLink . loop) =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: (Env m) => MVar TradeEnv -> m ()
loop varCfg = do
  cfg <- liftIO $ readMVar varCfg
  sleepPriceTtl $ tradeEnvCurrencyPair cfg
  withOperativeBfx $ createUpdate cfg
  loop varCfg

createUpdate :: (Env m) => TradeEnv -> m ()
createUpdate cfg = do
  res <-
    runExceptT . withExceptT ErrorBfx $
      (,) <$> getPrice @'Bfx.Buy <*> getPrice @'Bfx.Sell
  case res of
    Left e -> do
      $(logTM) ErrorS $ show e
      sleep [seconds|60|]
    Right (buy, sell) -> do
      priceEnt@(Entity _ price) <-
        Price.createUpdate sym buy sell
      when (priceUpdatedAt price == priceInsertedAt price) $
        putCurrPrice priceEnt
  where
    sym :: Bfx.CurrencyPair
    sym =
      tradeEnvCurrencyPair cfg
    getPrice ::
      forall (act :: Bfx.ExchangeAction) m.
      ( Env m,
        SingI act,
        Bfx.ToRequestParam (Bfx.Money 'Bfx.Base act)
      ) =>
      ExceptT Bfx.Error m (Bfx.QuotePerBase act)
    getPrice = do
      Bfx.marketAveragePrice
        ( case sing :: Sing act of
            Bfx.SBuy -> tradeEnvMinBuyAmt cfg
            Bfx.SSell -> tradeEnvMinSellAmt cfg
        )
        sym
