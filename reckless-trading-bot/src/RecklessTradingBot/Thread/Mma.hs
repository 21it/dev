{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Mma
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Trading as Bfx
import RecklessTradingBot.Import

apply :: (Env m) => m ()
apply = do
  $(logTM) DebugS "Spawned"
  forever $ do
    eMma0 <-
      runExceptT
        . Bfx.theBestMma Bfx.Ctf1m [moneyQuoteBuy|1|]
        $ Bfx.CurrencyCode "BTC"
    eMma1 <-
      case eMma0 of
        Right {} ->
          pure eMma0
        Left {} ->
          runExceptT
            . Bfx.theBestMma Bfx.Ctf1m [moneyQuoteBuy|30000|]
            $ Bfx.CurrencyCode "USD"
    case eMma1 of
      Right mma -> do
        $(logTM) DebugS . logStr $
          "Found good Mma for " <> inspect (Bfx.mmaSymbol mma)
        putCurrMma mma
        sleep normalCoolDown
      Left e@Bfx.ErrorTrading {} -> do
        $(logTM) DebugS . logStr $
          "Found normal trading failure " <> (show e :: Text)
        sleep normalCoolDown
      Left e -> do
        $(logTM) ErrorS . logStr $
          "Found critical bfx failure " <> (show e :: Text)
        sleep failureCoolDown

normalCoolDown :: Seconds
normalCoolDown =
  [seconds|30|]

failureCoolDown :: Seconds
failureCoolDown =
  [seconds|300|]
