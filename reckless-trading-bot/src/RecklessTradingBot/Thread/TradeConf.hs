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
import Data.Metrology.Poly
import RecklessTradingBot.Import

apply :: (Env m) => m ()
apply = do
  $(logTM) DebugS "Spawned"
  loop

loop :: (Env m) => m ()
loop = do
  reportProfit
  xs <- getPairs
  withOperativeBfx $ do
    res <-
      runExceptT $ do
        syms <- withExceptT ErrorBfx Bfx.symbolsDetails
        fees <- withBfxT Bfx.feeSummary id
        mapM_ (updateTradeConf syms fees) xs
    whenLeft res $
      $(logTM) ErrorS . show
    sleep [seconds|86400|]
  loop

reportProfit :: (Env m) => m ()
reportProfit = do
  cc <- getReportCurrency
  startAmt <- Bfx.unMoney <$> getReportStartAmt
  res <- runExceptT $ do
    currentAmt <-
      Bfx.unMoney <$> withBfxT Bfx.netWorth ($ cc)
    $(logTM) InfoS . logStr $
      ("Total profit is " :: Text)
        <> showPercent
          ((currentAmt |-| startAmt) |/| startAmt # Number)
  whenLeft res $ \e ->
    $(logTM) ErrorS . logStr $
      "Profit report failed with " <> (show e :: Text)

updateTradeConf ::
  ( MonadIO m
  ) =>
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  BfxFeeSummary.Response ->
  MVar TradeEnv ->
  ExceptT Error m ()
updateTradeConf syms fees varCfg = do
  cfg <- lift $ readMVar varCfg
  let sym = tradeEnvCurrencyPair cfg
  let cck = tradeEnvCurrencyKind cfg
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
      BfxMath.tweakMoneyPip
        =<< BfxMath.addFee amtNoFee fee
  void
    . liftIO
    . swapMVar varCfg
    $ TradeEnv
      { tradeEnvCurrencyPair = sym,
        tradeEnvCurrencyKind = cck,
        tradeEnvMinProfitPerOrder =
          tradeEnvMinProfitPerOrder cfg,
        tradeEnvMaxQuoteInvestment =
          tradeEnvMaxQuoteInvestment cfg,
        tradeEnvBaseFee = fee,
        tradeEnvQuoteFee = coerce fee,
        tradeEnvMinBuyAmt = amtWithFee,
        tradeEnvMinSellAmt = coerce amtNoFee,
        tradeEnvMode = tradeEnvMode cfg
      }
