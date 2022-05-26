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
import qualified Math.Combinat.Sets as Math

newtype ApproxProfitRate = ApproxProfitRate
  { unApproxProfitRate :: Rational
  }
  deriving newtype
    ( Eq,
      Ord
    )
  deriving stock
    ( Generic
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
    ( Generic
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
    ( Generic
    )

newtype TradeExit = TradeExit
  { unTradeExit :: Candle
  }
  deriving newtype
    ( Eq,
      Ord
    )
  deriving stock
    ( Generic
    )

data Mma = Mma
  { mmaCurves :: Map MaPeriod (Map UTCTime Ma),
    mmaTrades :: [(TradeEntry, TradeExit)],
    mmaProfit :: ApproxProfitRate,
    --
    -- TODO : mmaPower (from profit rate and trades)
    --
    mmaEntry :: Maybe TradeEntry
  }
  deriving stock
    ( Eq,
      Ord,
      Generic
    )

mma :: NonEmpty Candle -> Mma
mma cs =
  maximum $
    [1 .. 15]
      >>= combineMaPeriods cs

combineMaPeriods :: NonEmpty Candle -> CrvQty -> NonEmpty Mma
combineMaPeriods cs qty =
  newMma cs
    <$> ( fromMaybe
            (error "Impossible empty combineMaPeriods")
            . nonEmpty
            . catMaybes
            . (nonEmpty <$>)
            $ Math.choose (unCrvQty qty) [1 .. 400]
        )

newMma :: NonEmpty Candle -> NonEmpty MaPeriod -> Mma
newMma cs ps =
  Mma
    { mmaCurves = Map.fromList $ from curves,
      mmaTrades = trades,
      mmaProfit = ApproxProfitRate $ 0 % 1,
      mmaEntry = Nothing
    }
  where
    curves =
      (\p -> (p, ma p cs)) <$> ps
    trades =
      newMmaTrades cs curves

newMmaTrades ::
  NonEmpty Candle ->
  NonEmpty (MaPeriod, Map UTCTime Ma) ->
  [(TradeEntry, TradeExit)]
newMmaTrades _cs _curves =
  --
  -- TODO : !!!
  --
  mempty
