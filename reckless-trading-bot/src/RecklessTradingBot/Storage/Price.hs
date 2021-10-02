{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Storage.Price
  ( create,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import

create ::
  Storage m =>
  Bfx.CurrencyPair ->
  ExchangeRate 'Bfx.Buy ->
  ExchangeRate 'Bfx.Sell ->
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
