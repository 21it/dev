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
  forever $ do
    reportProfit
    withOperativeBfx $ do
      res <-
        runExceptT $ do
          syms <- withExceptT ErrorBfx Bfx.symbolsDetails
          fees <- withBfxT Bfx.feeSummary id
          conf <- lift getTradeVar
          updateTradeConf syms fees conf
      whenLeft res $
        $(logTM) ErrorS . show
      sleep [seconds|86400|]

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
  MVar (Map Bfx.CurrencyPair TradeEnv) ->
  ExceptT Error m ()
updateTradeConf syms fees var = do
  cfg <-
    Map.fromList
      <$> mapM (uncurry $ newConf fees) (Map.assocs syms)
  void
    . liftIO
    $ swapMVar var cfg

newConf ::
  ( MonadIO m
  ) =>
  BfxFeeSummary.Response ->
  Bfx.CurrencyPair ->
  Bfx.CurrencyPairConf ->
  ExceptT Error m (Bfx.CurrencyPair, TradeEnv)
newConf fees sym cfg = do
  --
  -- TODO : unhardcode
  --
  let cck = Bfx.Stable
  let fee = BfxFeeSummary.getFee @'Bfx.Maker cck fees
  let amtNoFee = Bfx.currencyPairMinOrderAmt cfg
  when (amtNoFee <= [moneyBaseBuy|0|])
    . throwE
    . ErrorRuntime
    $ "Wrong " <> show amtNoFee
  amtWithFee <-
    tryErrorT $
      BfxMath.tweakMoneyPip
        =<< BfxMath.addFee amtNoFee fee
  pure
    ( sym,
      TradeEnv
        { tradeEnvCurrencyPair = sym,
          tradeEnvCurrencyKind = cck,
          tradeEnvBaseFee = fee,
          tradeEnvQuoteFee = coerce fee,
          tradeEnvMinBuyAmt = amtWithFee,
          tradeEnvMinSellAmt = coerce amtNoFee
        }
    )
