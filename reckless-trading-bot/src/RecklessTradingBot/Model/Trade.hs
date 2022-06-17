{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Trade
  ( createT,
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as P

createT ::
  ( Storage m
  ) =>
  Bfx.Mma ->
  ExceptT Error m (Entity Trade)
createT mma = do
  tp <-
    tryErrorT
      . Bfx.roundQuotePerBase'
      . Bfx.unTakeProfit
      $ Bfx.tradeEntryTakeProfit entry
  sl <-
    tryErrorT
      . Bfx.roundQuotePerBase'
      . Bfx.unStopLoss
      $ Bfx.tradeEntryStopLoss entry
  unless ((sl < coerce cmp) && (coerce cmp < tp)) $
    throwE $ ErrorRuntime "Bad trading data"
  ct <-
    liftIO getCurrentTime
  let next =
        Trade
          { tradeBase = Bfx.currencyPairBase sym,
            tradeQuote = Bfx.currencyPairQuote sym,
            tradeEntry = cmp,
            tradeTakeProfit = tp,
            tradeStopLoss = sl,
            tradeInsertedAt = ct,
            tradeUpdatedAt = ct
          }
  lift . runSql $ do
    id0 <- P.insert next
    pure $ Entity id0 next
  where
    sym = Bfx.mmaSymbol mma
    cmp = Bfx.candleClose $ Bfx.tradeEntryCandle entry
    entry = Bfx.mmaEntry mma
