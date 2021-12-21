{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.TradeConf (apply) where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.FeeSummary as BfxFeeSummary
import qualified BitfinexClient.Math as BfxMath
import qualified Data.Map as Map
import RecklessTradingBot.Import

apply :: (Env m) => m ()
apply = do
  $(logTM) InfoS "Spawned"
  loop

loop :: (Env m) => m ()
loop = do
  sleep [seconds|600|]
  xs <- getPairs
  res <-
    runExceptT $ do
      syms <- withExceptT ErrorBfx Bfx.symbolsDetails
      fees <- withBfxT Bfx.feeSummary id
      mapM_ (updateTradeConf syms fees) xs
  whenLeft res $
    $(logTM) ErrorS . show
  loop

updateTradeConf ::
  ( MonadIO m
  ) =>
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  BfxFeeSummary.Response ->
  MVar TradeConf ->
  ExceptT Error m ()
updateTradeConf syms fees varCfg = do
  cfg <- lift $ readMVar varCfg
  let sym = tradeConfPair cfg
  let cck = tradeConfCurrencyKind cfg
  let fee = BfxFeeSummary.getFee @'Bfx.Maker cck fees
  bfxCfg <-
    tryJust
      (ErrorRuntime $ "Missing CurrencyPair" <> show sym)
      $ Map.lookup sym syms
  let amt = Bfx.currencyPairMinOrderAmt bfxCfg
  when (amt <= from @(Ratio Natural) 0)
    . throwE
    . ErrorRuntime
    $ "Wrong MoneyAmt " <> show amt
  void
    . liftIO
    . swapMVar varCfg
    $ TradeConf
      { tradeConfPair = sym,
        tradeConfCurrencyKind = cck,
        tradeConfBaseFee = fee,
        tradeConfQuoteFee = Bfx.coerceQuoteFeeRate fee,
        tradeConfMinBuyAmt = BfxMath.applyFee amt fee,
        tradeConfMinSellAmt = Bfx.coerceSellMoneyAmt amt
      }
