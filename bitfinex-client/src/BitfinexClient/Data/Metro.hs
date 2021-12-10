{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Metro
  ( MoneyAmt (..),
    MoneyBase,
    MoneyBase',
    MoneyBaseAmt (..),
    MoneyQuote,
    MoneyQuote',
    MoneyQuoteAmt (..),
    QuotePerBase (..),
    QuotePerBase',
    quotePerBaseAmt,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External
import Data.Metrology.Poly as Metro
--
-- TODO : somehow remove unsafe
--
import qualified Data.Metrology.Unsafe as Unsafe
import qualified Prelude

data MoneyBaseDim -- = MoneyBaseDim

instance Dimension MoneyBaseDim

data MoneyQuoteDim -- = MoneyQuoteDim

instance Dimension MoneyQuoteDim

data MoneyBaseAmt = MoneyBaseAmt

instance Unit MoneyBaseAmt where
  type BaseUnit MoneyBaseAmt = Canonical
  type DimOfUnit MoneyBaseAmt = MoneyBaseDim

instance Show MoneyBaseAmt where
  show = const "MoneyBaseAmt"

data MoneyQuoteAmt = MoneyQuoteAmt

instance Unit MoneyQuoteAmt where
  type BaseUnit MoneyQuoteAmt = Canonical
  type DimOfUnit MoneyQuoteAmt = MoneyQuoteDim

instance Show MoneyQuoteAmt where
  show = const "MoneyQuoteAmt"

type LCSU' =
  MkLCSU
    '[ (MoneyBaseDim, MoneyBaseAmt),
       (MoneyQuoteDim, MoneyQuoteAmt)
     ]

type MoneyBase' =
  MkQu_DLN MoneyBaseDim LCSU' (Ratio Natural)

type MoneyQuote' =
  MkQu_DLN MoneyQuoteDim LCSU' (Ratio Natural)

--
-- MoneyAmt sugar
--

newtype MoneyAmt dim = MoneyAmt
  { unMoneyAmt :: MkQu_DLN dim LCSU' (Ratio Natural)
  }
  deriving stock (Eq, Ord)

type MoneyBase = MoneyAmt MoneyBaseDim

type MoneyQuote = MoneyAmt MoneyQuoteDim

--
-- TODO : derive some newtype instances to use directly
-- in dimentional expressions without coercing?
--

unQu :: Qu a lcsu n -> n
unQu (Unsafe.Qu x) = x

instance
  ( Show unit,
    Lookup dim LCSU' ~ unit
  ) =>
  Prelude.Show (MoneyAmt dim)
  where
  show x =
    show (unQu $ unMoneyAmt x)
      <> " "
      <> show (undefined :: unit)

instance From (Ratio Natural) (MoneyAmt dim) where
  from = MoneyAmt . Unsafe.Qu

instance From (MoneyAmt dim) (Ratio Natural) where
  from = unQu . unMoneyAmt

instance TryFrom Rational (MoneyAmt dim) where
  tryFrom = from @(Ratio Natural) `composeTryRhs` tryFrom

instance From (MoneyAmt dim) Rational where
  from = via @(Ratio Natural)

--
-- QuotePerBase sugar
--

type QuotePerBase' =
  MoneyQuote' %/ MoneyBase'

newtype QuotePerBase = QuotePerBase
  { unQuotePerBase :: QuotePerBase'
  }
  deriving stock
    ( Eq,
      Ord
    )

quotePerBaseAmt :: MoneyQuoteAmt :/ MoneyBaseAmt
quotePerBaseAmt = MoneyQuoteAmt :/ MoneyBaseAmt

instance Prelude.Show QuotePerBase where
  show x =
    show (unQuotePerBase x # quotePerBaseAmt)
      <> " "
      <> show quotePerBaseAmt

instance ToRequestParam QuotePerBase where
  toTextParam =
    toTextParam
      . from @QuotePerBase @(Ratio Natural)

instance TryFrom (Ratio Natural) QuotePerBase where
  tryFrom x
    | x > 0 = Right . QuotePerBase $ x Metro.% quotePerBaseAmt
    | otherwise = Left $ TryFromException x Nothing

instance From QuotePerBase (Ratio Natural) where
  from =
    (# quotePerBaseAmt) . unQuotePerBase

instance TryFrom Rational QuotePerBase where
  tryFrom =
    tryVia @(Ratio Natural)

instance From QuotePerBase Rational where
  from =
    via @(Ratio Natural)

--
-- TODO : add Buy/Sell phantom kind param
--
instance ToRequestParam (ExchangeAction, MoneyBase) where
  toTextParam (act, amt) =
    toTextParam $
      case act of
        Buy -> absAmt
        Sell -> (-1) * absAmt
    where
      absAmt = abs $ into @Rational amt
