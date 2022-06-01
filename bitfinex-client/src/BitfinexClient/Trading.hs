{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Trading
  ( theBestMma,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.Candles as Candles
import BitfinexClient.Import
import BitfinexClient.Indicator.Mma (Mma)
import qualified BitfinexClient.Indicator.Mma as Mma
import qualified Control.Parallel.Strategies as Par
import qualified Data.Map as Map

theBestMma ::
  ( MonadIO m
  ) =>
  CandleTimeFrame ->
  CurrencyCode 'Quote ->
  ExceptT Error m Mma
theBestMma ctf quote = do
  --
  -- TODO : filter symbols, trade high
  -- volume symbols only!!!
  --
  syms <-
    Bfx.symbolsDetails
  cs <-
    mapM
      ( \sym -> do
          liftIO $ threadDelay 100000
          (sym,)
            <$> Bfx.candlesHist
              ctf
              sym
              Candles.optsDef
                { Candles.limit = Just 1500
                }
      )
      . traceShowId
      . filter ((== quote) . currencyPairQuote)
      $ Map.keys syms
  case nonEmpty cs of
    Nothing ->
      throwE $
        ErrorTrading quote "Can not find trading pairs"
    Just ncs ->
      ExceptT
        . pure
        . maybeToRight (ErrorTrading quote "No any good Mma")
        . (maximum <$>)
        . nonEmpty
        . catMaybes
        . Par.withStrategy (Par.parTraversable Par.rdeepseq)
        $ uncurry Mma.mma <$> toList ncs
