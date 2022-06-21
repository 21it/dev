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
import qualified Data.List.NonEmpty.Extra as NE
import qualified Data.Map as Map

theBestMma ::
  ( MonadIO m
  ) =>
  CandleTimeFrame ->
  Money 'Quote 'Buy ->
  CurrencyCode 'Quote ->
  ExceptT Error m Mma
theBestMma ctf vol quote = do
  tickers <-
    Bfx.tickers
  let goodTickers =
        Map.filter
          ( \x ->
              ( ( unQuotePerBase (tickerBid x)
                    |*| unMoney @'Base (tickerVolume x)
                )
                  > unMoney vol
              )
                && ( currencyPairQuote
                       (tickerSymbol x)
                       == quote
                   )
          )
          tickers
  syms <-
    Bfx.symbolsDetails
  cs <-
    mapM
      ( \sym -> do
          liftIO $ threadDelay 500000
          (sym,)
            <$> Bfx.candlesHist
              ctf
              sym
              Candles.optsDef
                { Candles.limit = Just 1000
                }
      )
      . filter (`Map.member` goodTickers)
      $ Map.keys syms
  case nonEmpty cs of
    Nothing ->
      throwE $
        ErrorTrading quote "Can not find trading pairs"
    Just ncs -> do
      mma <-
        ExceptT
          . pure
          . maybeToRight (ErrorTrading quote "No any good Mma")
          . (maximum <$>)
          . nonEmpty
          . catMaybes
          . Par.withStrategy (Par.parTraversable Par.rdeepseq)
          $ uncurry (Mma.mma ctf) <$> toList ncs
      let sym =
            Mma.mmaSymbol mma
      cfg <-
        ExceptT
          . pure
          . maybeToRight
            ( ErrorTrading quote $
                "Can not find symbol datails for " <> show sym
            )
          $ Map.lookup sym syms
      avg <-
        Bfx.marketAveragePrice (currencyPairMinOrderAmt cfg) sym
      let candles =
            Mma.mmaCandles mma
      let history =
            init candles
      let entry =
            last candles
      ExceptT
        . pure
        . maybeToRight
          ( ErrorTrading quote $
              "Can not verify Mma for " <> show sym
          )
        . Mma.mma ctf sym
        $ NE.appendr history [entry {candleClose = avg}]
