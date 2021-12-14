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
import Data.Metrology.Poly as Metro
--
-- TODO : somehow remove unsafe
--
import qualified Data.Metrology.Unsafe as Unsafe
import GHC.Natural (naturalFromInteger)
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

newtype MoneyAmt dim (act :: ExchangeAction) = MoneyAmt
  { unMoneyAmt :: MkQu_DLN dim LCSU' (Ratio Natural)
  }
  deriving stock (Eq, Ord)

data SomeMoneyAmt dim
  = forall act.
    ( Show (MoneyAmt dim act)
    ) =>
    SomeMoneyAmt
      (Sing act)
      (MoneyAmt dim act)

deriving stock instance Show (SomeMoneyAmt dim)

instance Eq (SomeMoneyAmt dim) where
  (SomeMoneyAmt sx x) == (SomeMoneyAmt sy y) =
    case eqExchangeAction sx sy of
      Just Refl -> x == y
      Nothing -> False

type MoneyBase = MoneyAmt MoneyBaseDim

type MoneyQuote = MoneyAmt MoneyQuoteDim

type SomeMoneyBase = SomeMoneyAmt MoneyBaseDim

type SomeMoneyQuote = SomeMoneyAmt MoneyQuoteDim

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
  Prelude.Show (MoneyAmt dim act)
  where
  show x =
    show (unQu $ unMoneyAmt x)
      <> " "
      <> show (undefined :: unit)

--
-- TODO : remove (Ratio Natural) instances, because
-- they are lossy in general case (MoneyAmt dim act).
-- Maybe leave (MoneyAmt dim 'Buy) instances because
-- they are not lossy.
--
instance From (Ratio Natural) (MoneyAmt dim act) where
  from = MoneyAmt . Unsafe.Qu

instance From (MoneyAmt dim act) (Ratio Natural) where
  from = unQu . unMoneyAmt

instance TryFrom Rational (MoneyAmt dim act) where
  tryFrom = from @(Ratio Natural) `composeTryRhs` tryFrom

instance From (MoneyAmt dim act) Rational where
  from = via @(Ratio Natural)

instance
  ( Show (Lookup dim LCSU')
  ) =>
  TryFrom Rational (SomeMoneyAmt dim)
  where
  tryFrom = \case
    rat
      | rat > 0 ->
        Right
          . SomeMoneyAmt (sing :: Sing 'Buy)
          . MoneyAmt
          . Unsafe.Qu
          $ nat rat
    rat
      | rat < 0 ->
        Right
          . SomeMoneyAmt (sing :: Sing 'Sell)
          . MoneyAmt
          . Unsafe.Qu
          $ nat rat
    rat ->
      Left $
        TryFromException rat Nothing
    where
      nat x =
        (naturalFromInteger . abs $ numerator x)
          Ext.% (naturalFromInteger . abs $ denominator x)

--
-- QuotePerBase sugar
--

type QuotePerBase' =
  MoneyQuote' %/ MoneyBase'

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

instance ToRequestParam (MoneyBase 'Buy) where
  toTextParam =
    toTextParam
      . abs
      . into @Rational

instance ToRequestParam (MoneyBase 'Sell) where
  toTextParam =
    toTextParam
      . ((-1) *)
      . abs
      . into @Rational

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

instance From SomeQuotePerBase (Ratio Natural) where
  from (SomeQuotePerBase _ x) = from x
