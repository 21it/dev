{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Indicator.Mma
  ( ApproxProfitRate (..),
    TradeEntry (..),
    TradeExit (..),
    Mma (..),
    mma,
  )
where

import BitfinexClient.Import
import BitfinexClient.Indicator.Ma
import qualified Data.List.Index as Index
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Math.Combinat.Sets as Math

newtype ApproxProfitRate = ApproxProfitRate
  { unApproxProfitRate :: Rational
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

newtype CrvQty = CrvQty
  { unCrvQty :: Int
  }
  deriving newtype
    ( Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral
    )
  deriving stock
    ( Generic,
      Show
    )

--
-- TODO : Introduce trade entry delay
-- to cover more possibilities of trade entry
-- X candles after indicator.
--

newtype TradeEntry = TradeEntry
  { unTradeEntry :: Candle
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

newtype TradeExit = TradeExit
  { unTradeExit :: Candle
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic,
      Show
    )

data Mma = Mma
  { mmaSymbol :: CurrencyPair,
    mmaCandles :: NonEmpty Candle,
    mmaCurves :: Map MaPeriod (Map UTCTime Ma),
    mmaTrades :: [(TradeEntry, TradeExit)],
    mmaProfit :: ApproxProfitRate,
    --
    -- TODO : mmaPower (from profit rate and trades)
    -- use this in Ord instance!!!
    --
    mmaEntry :: Maybe TradeEntry
  }
  deriving stock
    ( Eq,
      Generic
    )

instance NFData Mma

instance Ord Mma where
  compare lhs rhs =
    case (mmaEntry lhs, mmaEntry rhs) of
      (Nothing, Nothing) -> ordByTrades
      (Nothing, Just {}) -> LT
      (Just {}, Nothing) -> GT
      (Just {}, Just {}) -> ordByTrades
    where
      ordByTrades =
        compare
          (length $ mmaTrades lhs)
          (length $ mmaTrades rhs)

mma :: CurrencyPair -> NonEmpty Candle -> Mma
mma sym cs =
  maximum $
    [3 .. 5]
      >>= combineMaPeriods sym cs

combineMaPeriods ::
  CurrencyPair ->
  NonEmpty Candle ->
  CrvQty ->
  NonEmpty Mma
combineMaPeriods sym cs qty =
  fromMaybe
    (error "Impossible empty combineMaPeriods")
    . nonEmpty
    . fmap (newMma sym cs)
    . catMaybes
    . (nonEmpty <$>)
    . Math.choose (unCrvQty qty)
    -- <$> [5, 20 .. 200]
    $ (\p -> (p, ma p cs)) <$> [5, 25 .. 200]

newMma ::
  CurrencyPair ->
  NonEmpty Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  Mma
newMma sym cs curves =
  Mma
    { mmaSymbol = sym,
      mmaCandles = cs,
      mmaCurves = Map.fromList $ from curves,
      mmaTrades = trades,
      mmaProfit = profit,
      mmaEntry =
        traceShow (fst <$> curves) $
          if notNull trades
            && (length rawTrades == length mTrades + 1)
            then Just entry
            else Nothing
    }
  where
    --
    -- TODO : make profit rate various
    --
    entry =
      TradeEntry $ last cs
    profit =
      ApproxProfitRate $ 1 % 500
    rawTrades =
      newMmaTrades profit cs curves
    mTrades =
      filter ((/= entry) . fst) rawTrades
    closedTrades =
      mapMaybe (\(x, y) -> (x,) <$> y) mTrades
    trades =
      if length mTrades == length closedTrades
        then closedTrades
        else mempty

newMmaTrades ::
  ApproxProfitRate ->
  NonEmpty Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  [(TradeEntry, Maybe TradeExit)]
newMmaTrades profit cs curves =
  foldl
    ( \acc (idx, (c0, c1)) ->
        let at0 = candleAt c0
            mas0 = newMas at0
            at1 = candleAt c1
            mas1 = newMas at1
            entry = TradeEntry c1
         in if (length curves == length mas0)
              && (length curves == length mas1)
              && goodCandle c0 c1 mas1
              && not (goodCandle c0 c1 mas0)
              then (entry, tryFindExit idx profit entry cs) : acc
              else acc
    )
    mempty
    . Index.indexed
    . zip (toList cs)
    $ tail cs
  where
    newMas :: UTCTime -> [(MaPeriod, Ma)]
    newMas t =
      mapMaybe
        (\(p, curve) -> (p,) <$> Map.lookup t curve)
        $ toList curves

goodCandle ::
  Candle ->
  Candle ->
  [(MaPeriod, Ma)] ->
  Bool
goodCandle x0 x1 xs =
  (c1 > c0)
    && all ((unQuotePerBase c1 >) . unMa . snd) xs
    && (xs == sortOn (Ord.Down . snd) xs)
  where
    c0 = candleClose x0
    c1 = candleClose x1

tryFindExit ::
  Int ->
  ApproxProfitRate ->
  TradeEntry ->
  NonEmpty Candle ->
  Maybe TradeExit
tryFindExit idx profit entry =
  (TradeExit <$>)
    . find
      ( \x ->
          (unQuotePerBase (candleClose x) > sell)
            && candleAt x > entryAt
      )
    . drop idx
    . toList
  where
    entryCandle = unTradeEntry entry
    entryAt = candleAt entryCandle
    buy = candleClose entryCandle
    sell = unQuotePerBase buy |* (1 + unApproxProfitRate profit)
