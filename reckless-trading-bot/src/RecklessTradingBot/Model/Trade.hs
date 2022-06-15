{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Trade
  ( createUpdate,
    getLatest,
    getLatestLimit,
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as P

createUpdate ::
  ( Storage m
  ) =>
  Bfx.Mma ->
  ExceptT Error m (Entity Trade)
createUpdate mma = do
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
  mPrev <-
    lift $ getLatest sym
  lift . runSql $
    case mPrev of
      Just (Entity id0 prev) | theSamePrice prev next -> do
        P.update $ \row -> do
          P.set row [TradeUpdatedAt P.=. P.val ct]
          P.where_ $ row P.^. TradeId P.==. P.val id0
        pure . Entity id0 $
          prev
            { tradeUpdatedAt = ct
            }
      _ -> do
        id0 <- P.insert next
        pure $ Entity id0 next
  where
    sym = Bfx.mmaSymbol mma
    cmp = Bfx.candleClose $ Bfx.tradeEntryCandle entry
    entry = Bfx.mmaEntry mma

theSamePrice :: Trade -> Trade -> Bool
theSamePrice x y =
  x
    { tradeInsertedAt = tradeInsertedAt y,
      tradeUpdatedAt = tradeUpdatedAt y
    }
    == y

getLatest ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m (Maybe (Entity Trade))
getLatest =
  (listToMaybe <$>)
    . getLatestLimit 1

getLatestLimit ::
  ( Storage m
  ) =>
  Int64 ->
  Bfx.CurrencyPair ->
  m [Entity Trade]
getLatestLimit lim sym = do
  xs <-
    runSql $
      P.select $
        P.from $ \row -> do
          P.where_
            ( ( row P.^. TradeBase
                  P.==. P.val
                    ( Bfx.currencyPairBase sym
                    )
              )
                P.&&. ( row P.^. TradeQuote
                          P.==. P.val
                            ( Bfx.currencyPairQuote sym
                            )
                      )
            )
          P.orderBy [P.desc $ row P.^. TradeId]
          P.limit lim
          pure row
  pure $
    sortBy
      ( \lhs rhs ->
          compare
            (entityKey lhs)
            (entityKey rhs)
      )
      xs
