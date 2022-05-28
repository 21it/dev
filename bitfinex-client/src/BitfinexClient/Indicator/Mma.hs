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
import qualified Math.Combinat.Sets as Math

newtype ApproxProfitRate = ApproxProfitRate
  { unApproxProfitRate :: Rational
  }
  deriving newtype
    ( Eq,
      Ord
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
      Ord
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
      Ord
    )
  deriving stock
    ( Generic,
      Show
    )

data Mma = Mma
  { mmaCurves :: Map MaPeriod (Map UTCTime Ma),
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

mma :: NonEmpty Candle -> Mma
mma cs =
  maximum $
    [3 .. 5]
      >>= combineMaPeriods cs

combineMaPeriods :: NonEmpty Candle -> CrvQty -> NonEmpty Mma
combineMaPeriods cs qty =
  --
  -- TODO : pre-calculate all MAs!!!
  --
  newMma cs
    <$> ( fromMaybe
            (error "Impossible empty combineMaPeriods")
            . nonEmpty
            . catMaybes
            . (nonEmpty <$>)
            $ Math.choose (unCrvQty qty) [10, 50 .. 200]
        )

newMma :: NonEmpty Candle -> NonEmpty MaPeriod -> Mma
newMma cs ps =
  Mma
    { mmaCurves =
        Map.fromList $ from curves,
      mmaTrades =
        if length mTrades == length trades
          then trades
          else mempty,
      mmaProfit =
        profit,
      mmaEntry =
        if length rawTrades == length mTrades + 1
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
    curves =
      (\p -> (p, ma p cs)) <$> traceShowId ps
    rawTrades =
      newMmaTrades profit cs curves
    mTrades =
      filter ((/= entry) . fst) rawTrades
    trades =
      mapMaybe (\(x, y) -> (x,) <$> y) mTrades

newMmaTrades ::
  ApproxProfitRate ->
  NonEmpty Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  [(TradeEntry, Maybe TradeExit)]
newMmaTrades profit cs curves =
  foldl
    ( \acc (c0, c1) ->
        let at0 = candleAt c0
            mas0 = newMas at0
            at1 = candleAt c1
            mas1 = newMas at1
            entry = TradeEntry c1
         in if (length curves == length mas0)
              && (length curves == length mas1)
              && not (goodCandle mas0)
              && goodCandle mas1
              then (entry, tryFindExit profit entry cs) : acc
              else acc
    )
    mempty
    . zip (toList cs)
    $ tail cs
  where
    newMas :: UTCTime -> [(MaPeriod, Ma)]
    newMas t =
      mapMaybe
        (\(p, curve) -> (p,) <$> Map.lookup t curve)
        $ toList curves

goodCandle :: [(MaPeriod, Ma)] -> Bool
goodCandle xs =
  sortOn fst xs == sortOn (Ord.Down . snd) xs

tryFindExit ::
  ApproxProfitRate ->
  TradeEntry ->
  NonEmpty Candle ->
  Maybe TradeExit
tryFindExit profit entry =
  (TradeExit <$>)
    . find ((> sell) . unQuotePerBase . candleClose)
    . filter ((> entryAt) . candleAt)
    . toList
  where
    entryCandle = unTradeEntry entry
    entryAt = candleAt entryCandle
    buy = candleClose entryCandle
    sell = unQuotePerBase buy |* (1 + unApproxProfitRate profit)
