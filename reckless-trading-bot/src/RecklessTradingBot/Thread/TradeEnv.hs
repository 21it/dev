{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.TradeEnv
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.FeeSummary as BfxFeeSummary
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
          fee <- withBfxT Bfx.feeSummary id
          sym <- withExceptT ErrorBfx Bfx.symbolsDetails
          var <- lift getTradeVar
          updateTradeEnv fee sym var
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

updateTradeEnv ::
  ( MonadIO m
  ) =>
  BfxFeeSummary.Response ->
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  MVar (Map Bfx.CurrencyPair TradeEnv) ->
  ExceptT Error m ()
updateTradeEnv fee sym var = do
  env <- except . first ErrorBfx $ newTradeEnv fee sym
  void . liftIO $ swapMVar var env
