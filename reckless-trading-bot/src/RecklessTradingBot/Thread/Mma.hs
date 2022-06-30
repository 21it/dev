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
import qualified RecklessTradingBot.Model.Order as Order
import qualified System.Clock as Clock

apply :: (Env m) => m ()
apply = do
  $(logTM) DebugS "Spawned"
  blacklist <- getBaseBlacklist
  forever $ do
    --
    -- TODO : Check if bfx fixes the bug where
    -- OCO flag still locks additional liquidity.
    -- If it's fixed - then this additional
    -- blacklist from ongoing trades is not needed.
    --
    ongoing <- Order.getNonCountered
    (eMma, tc) <-
      stopWatch
        . runExceptT
        . Bfx.theBestMma
          (Bfx.ProfitRateB @'Bfx.Min $ Bfx.ProfitRate 0.005)
          (Bfx.ProfitRateB @'Bfx.Max $ Bfx.ProfitRate 0.05)
          Bfx.Ctf1m
          [moneyQuoteBuy|0.5|]
          ( fromList
              ( tradeBase
                  . entityVal
                  . snd
                  <$> ongoing
              )
              <> blacklist
          )
        $ Bfx.CurrencyCode "BTC"
    let sec = Clock.sec tc
    let lvl = if sec > 240 then ErrorS else InfoS
    $(logTM) lvl . logStr $
      "Mma calculation time = " <> inspect sec <> " seconds"
    case eMma of
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
