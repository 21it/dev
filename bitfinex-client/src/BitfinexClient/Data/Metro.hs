{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Metro
  ( MoneyAmt (..),
    SomeMoneyAmt (..),
    MoneyBase,
    MoneyBase',
    MoneyBaseAmt (..),
    SomeMoneyBase,
    MoneyQuote,
    MoneyQuote',
    MoneyQuoteAmt (..),
    SomeMoneyQuote,
    QuotePerBase (..),
    QuotePerBase',
    SomeQuotePerBase (..),
    quotePerBaseAmt,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External as Ext
import BitfinexClient.Util
import Data.Metrology.Poly as Metro
--
-- TODO : somehow remove unsafe
--
import qualified Data.Metrology.Unsafe as Unsafe
import qualified Prelude

data MoneyBaseDim

instance Dimension MoneyBaseDim

data MoneyQuoteDim

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

newtype MoneyAmt dim (act :: ExchangeAction) = MoneyAmt
  { unMoneyAmt :: MkQu_DLN dim LCSU' (Ratio Natural)
  }
  deriving stock
    ( Eq,
      Ord
    )

instance
  ( Show unit,
    Lookup dim LCSU' ~ unit
  ) =>
  Prelude.Show (MoneyAmt dim act)
  where
  show x =
    show (unQu $ unMoneyAmt x)
      <> " "
      <> show (undefined :: unit)

data SomeMoneyAmt dim
  = forall act.
    ( Show (MoneyAmt dim act)
    ) =>
    SomeMoneyAmt
      (Sing act)
      (MoneyAmt dim act)

instance Eq (SomeMoneyAmt dim) where
  (SomeMoneyAmt sx x) == (SomeMoneyAmt sy y) =
    case eqExchangeAction sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoneyAmt dim)

type MoneyBase = MoneyAmt MoneyBaseDim

type MoneyQuote = MoneyAmt MoneyQuoteDim

type SomeMoneyBase = SomeMoneyAmt MoneyBaseDim

type SomeMoneyQuote = SomeMoneyAmt MoneyQuoteDim

instance (SingI act) => ToRequestParam (MoneyBase act) where
  toTextParam =
    toTextParam . into @Rational

-- | Dumb constructors
instance From (Ratio Natural) (MoneyAmt dim act) where
  from =
    MoneyAmt . Unsafe.Qu

instance From (MoneyAmt dim act) (Ratio Natural) where
  from =
    unQu . unMoneyAmt

-- | Smart constructors
instance (SingI act) => TryFrom Rational (MoneyAmt dim act) where
  tryFrom rat =
    case sing :: Sing act of
      SBuy | rat >= 0 -> success rat
      SSell | rat <= 0 -> success rat
      _ -> Left $ TryFromException rat Nothing
    where
      success = Right . MoneyAmt . Unsafe.Qu . absRat

instance (SingI act) => From (MoneyAmt dim act) Rational where
  from amt =
    case sing :: Sing act of
      SBuy -> success amt
      SSell -> (-1) * success amt
    where
      success :: MoneyAmt dim act -> Rational
      success = abs . from . unQu . unMoneyAmt

instance
  ( Show (Lookup dim LCSU')
  ) =>
  From Rational (SomeMoneyAmt dim)
  where
  from rat =
    --
    -- TODO : should it be TryFrom to exclude 0?
    --
    if rat >= 0
      then
        SomeMoneyAmt (sing :: Sing 'Buy)
          . MoneyAmt
          . Unsafe.Qu
          $ absRat rat
      else
        SomeMoneyAmt (sing :: Sing 'Sell)
          . MoneyAmt
          . Unsafe.Qu
          $ absRat rat

--
-- QuotePerBase sugar
--

type QuotePerBase' = MoneyQuote' %/ MoneyBase'

newtype QuotePerBase (act :: ExchangeAction) = QuotePerBase
  { unQuotePerBase :: QuotePerBase'
  }
  deriving stock
    ( Eq,
      Ord
    )

quotePerBaseAmt :: MoneyQuoteAmt :/ MoneyBaseAmt
quotePerBaseAmt = MoneyQuoteAmt :/ MoneyBaseAmt

--
-- TODO : show act as well?
--
instance Prelude.Show (QuotePerBase act) where
  show x =
    show (unQuotePerBase x # quotePerBaseAmt)
      <> " "
      <> show quotePerBaseAmt

instance ToRequestParam (QuotePerBase act) where
  toTextParam =
    toTextParam
      . from @(QuotePerBase act) @(Ratio Natural)

instance TryFrom (Ratio Natural) (QuotePerBase act) where
  tryFrom x
    | x > 0 = Right . QuotePerBase $ x Metro.% quotePerBaseAmt
    | otherwise = Left $ TryFromException x Nothing

instance From (QuotePerBase act) (Ratio Natural) where
  from =
    (# quotePerBaseAmt) . unQuotePerBase

instance TryFrom Rational (QuotePerBase act) where
  tryFrom =
    tryVia @(Ratio Natural)

instance From (QuotePerBase act) Rational where
  from =
    via @(Ratio Natural)

--
-- SomeQuotePerBase sugar
--

data SomeQuotePerBase :: Type where
  SomeQuotePerBase ::
    Sing act ->
    QuotePerBase act ->
    SomeQuotePerBase

deriving stock instance Show SomeQuotePerBase

instance Eq SomeQuotePerBase where
  (SomeQuotePerBase sx x) == (SomeQuotePerBase sy y) =
    case eqExchangeAction sx sy of
      Just Refl -> x == y
      Nothing -> False

--
-- TODO : derive some newtype instances to use directly
-- in dimentional expressions without coercing?
--

unQu :: Qu a lcsu n -> n
unQu (Unsafe.Qu x) = x
