{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.TradeConf
  ( apply,
  )
where

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
  xs <- getPairs
  res <-
    runExceptT $ do
      syms <- withExceptT ErrorBfx Bfx.symbolsDetails
      fees <- withBfxT Bfx.feeSummary id
      mapM_ (updateTradeConf syms fees) xs
  whenLeft res $
    $(logTM) ErrorS . show
  sleep [seconds|300|]
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
  let sym = tradeConfCurrencyPair cfg
  let cck = tradeConfCurrencyKind cfg
  let fee = BfxFeeSummary.getFee @'Bfx.Maker cck fees
  bfxCfg <-
    tryJust
      (ErrorRuntime $ "Missing " <> show sym)
      $ Map.lookup sym syms
  let amtNoFee = Bfx.currencyPairMinOrderAmt bfxCfg
  when (amtNoFee <= [moneyBaseBuy|0|])
    . throwE
    . ErrorRuntime
    $ "Wrong " <> show amtNoFee
  amtWithFee <-
    tryErrorT $
      BfxMath.addFee amtNoFee fee
  void
    . liftIO
    . swapMVar varCfg
    $ TradeConf
      { tradeConfCurrencyPair = sym,
        tradeConfCurrencyKind = cck,
        tradeConfMinProfitPerOrder =
          tradeConfMinProfitPerOrder cfg,
        tradeConfMaxQuoteInvestment =
          tradeConfMaxQuoteInvestment cfg,
        tradeConfBaseFee = fee,
        tradeConfQuoteFee = coerce fee,
        tradeConfMinBuyAmt = amtWithFee,
        tradeConfMinSellAmt = coerce amtNoFee
      }
