{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Price
  ( create,
    getSeq,
    getLatest,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import

create ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  QuotePerBase 'Bfx.Buy ->
  QuotePerBase 'Bfx.Sell ->
  m (Entity Price)
create pair buy sell = do
  x <- liftIO $ newPrice <$> getCurrentTime
  id0 <- runSql $ P.insert x
  pure $ Entity id0 x
  where
    newPrice ct =
      Price
        { priceBase =
            CurrencyCode $
              Bfx.currencyPairBase pair,
          priceQuote =
            CurrencyCode $
              Bfx.currencyPairQuote pair,
          priceBuy = buy,
          priceSell = sell,
          priceAt = ct
        }

getSeq ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m [Entity Price]
getSeq pair =
  runSql $
    P.selectList
      [ PriceBase
          P.==. CurrencyCode
            (Bfx.currencyPairBase pair),
        PriceQuote
          P.==. CurrencyCode
            (Bfx.currencyPairQuote pair)
      ]
      [ P.Desc PriceId,
        P.LimitTo 3
      ]

getLatest ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m (Maybe (Entity Price))
getLatest pair =
  runSql $
    listToMaybe
      <$> P.selectList
        [ PriceBase
            P.==. CurrencyCode
              (Bfx.currencyPairBase pair),
          PriceQuote
            P.==. CurrencyCode
              (Bfx.currencyPairQuote pair)
        ]
        [ P.Desc PriceId,
          P.LimitTo 1
        ]
