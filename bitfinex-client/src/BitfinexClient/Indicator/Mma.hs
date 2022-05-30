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
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Vector as V
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
    mmaEntry :: TradeEntry
  }
  deriving stock
    ( Eq,
      Generic
    )

instance NFData Mma

instance Ord Mma where
  compare lhs rhs =
    compare
      ( via @Integer (length $ mmaTrades lhs)
          * unApproxProfitRate (mmaProfit lhs)
      )
      ( via @Integer (length $ mmaTrades rhs)
          * unApproxProfitRate (mmaProfit rhs)
      )

mma :: CurrencyPair -> NonEmpty Candle -> Maybe Mma
mma sym cs =
  (maximum <$>) . nonEmpty $
    [3 .. 5]
      >>= combineMaPeriods sym cs

combineMaPeriods ::
  CurrencyPair ->
  NonEmpty Candle ->
  CrvQty ->
  [Mma]
combineMaPeriods sym cs qty =
  mapMaybe (newMma sym cs . V.fromList $ toList cs)
    . catMaybes
    . (nonEmpty <$>)
    . Math.choose (unCrvQty qty)
    $ (\p -> (p, ma p cs)) <$> [5, 25 .. 200]

newMma ::
  CurrencyPair ->
  NonEmpty Candle ->
  Vector Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  Maybe Mma
newMma sym cs0 cs curves = do
  (csHistory, cLast) <- V.unsnoc cs
  (_, cPrev) <- V.unsnoc csHistory
  entry <-
    newMmaEntries [cPrev, cLast] curves V.!? 0
  (maximum <$>)
    . nonEmpty
    . catMaybes
    $ ( \den -> do
          let profit = ApproxProfitRate $ 1 % den
          trades <-
            V.imapM (tryFindExit profit csHistory) $
              newMmaEntries csHistory curves
          pure
            Mma
              { mmaSymbol = sym,
                mmaCandles = cs0,
                mmaCurves = Map.fromList $ from curves,
                mmaTrades = V.toList trades,
                mmaProfit = profit,
                mmaEntry = entry
              }
      )
      <$> [50, 100 .. 500]

newMmaEntries ::
  Vector Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  Vector TradeEntry
newMmaEntries cs curves =
  ( \(c0, c1) ->
      let at0 = candleAt c0
          mas0 = newMas at0
          at1 = candleAt c1
          mas1 = newMas at1
       in if (length curves == length mas0)
            && (length curves == length mas1)
            && goodCandle c0 c1 mas1
            && not (goodCandle c0 c1 mas0)
            then V.singleton $ TradeEntry c1
            else mempty
  )
    <=< V.zip cs
    $ V.tail cs
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
  ApproxProfitRate ->
  Vector Candle ->
  Int ->
  TradeEntry ->
  Maybe (TradeEntry, TradeExit)
tryFindExit profit cs idx entry =
  ((entry,) . TradeExit <$>)
    . find
      ( \x ->
          (candleAt x < exitUntil)
            && unQuotePerBase (candleClose x) > sell
            && candleAt x > entryAt
      )
    . drop idx
    $ toList cs
  where
    entryCandle = unTradeEntry entry
    entryAt = candleAt entryCandle
    exitUntil = addUTCTime nominalDay entryAt
    buy = candleClose entryCandle
    sell = unQuotePerBase buy |* (1 + unApproxProfitRate profit)
