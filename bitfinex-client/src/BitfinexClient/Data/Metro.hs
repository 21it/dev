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

type MoneyBase = MoneyAmt MoneyBaseDim

type MoneyQuote = MoneyAmt MoneyQuoteDim

instance (SingI act) => ToRequestParam (MoneyBase act) where
  toTextParam amt =
    case sing :: Sing act of
      SBuy -> toTextParam $ success amt
      SSell -> toTextParam $ (-1) * success amt
    where
      success :: MoneyAmt dim act -> Rational
      success = abs . from . unQu . unMoneyAmt

instance From (Ratio Natural) (MoneyAmt dim act) where
  from =
    MoneyAmt . Unsafe.Qu

instance From (MoneyAmt dim act) (Ratio Natural) where
  from =
    unQu . unMoneyAmt

instance TryFrom Rational (MoneyAmt dim act) where
  tryFrom =
    from @(Ratio Natural) `composeTryRhs` tryFrom

instance From (MoneyAmt dim act) Rational where
  from =
    via @(Ratio Natural)

deriving via
  Rational
  instance
    ( SingI act
    ) =>
    PersistFieldSql (MoneyAmt dim act)

instance
  ( SingI act
  ) =>
  PersistField (MoneyAmt dim act)
  where
  toPersistValue =
    PersistRational . via @(Ratio Natural)
  fromPersistValue raw =
    case raw of
      PersistRational x ->
        first (const failure) $
          from @(Ratio Natural) `composeTryRhs` tryFrom $ x
      _ ->
        Left failure
    where
      failure =
        "MoneyAmt "
          <> show (fromSing (sing :: Sing act))
          <> " PersistValue is invalid "
          <> show raw

--
-- SomeMoneyAmt sugar
--

data SomeMoneyAmt dim
  = forall act.
    ( Show (MoneyAmt dim act),
      SingI act
    ) =>
    SomeMoneyAmt
      (Sing act)
      (MoneyAmt dim act)

type SomeMoneyBase = SomeMoneyAmt MoneyBaseDim

type SomeMoneyQuote = SomeMoneyAmt MoneyQuoteDim

instance Eq (SomeMoneyAmt dim) where
  (SomeMoneyAmt sx x) == (SomeMoneyAmt sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoneyAmt dim)

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
instance (SingI act) => Prelude.Show (QuotePerBase act) where
  show x =
    show (unQuotePerBase x # quotePerBaseAmt)
      <> " "
      <> show quotePerBaseAmt
      <> " "
      <> show (fromSing (sing :: Sing act))

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

deriving via
  Rational
  instance
    PersistFieldSql (QuotePerBase act)

instance PersistField (QuotePerBase act) where
  toPersistValue =
    PersistRational . from
  fromPersistValue raw =
    case raw of
      PersistRational x -> first (const failure) $ tryFrom x
      _ -> Left failure
    where
      failure =
        "QuotePerBase PersistValue is invalid " <> show raw

--
-- SomeQuotePerBase sugar
--

data SomeQuotePerBase :: Type where
  SomeQuotePerBase ::
    ( SingI act
    ) =>
    Sing act ->
    QuotePerBase act ->
    SomeQuotePerBase

instance Eq SomeQuotePerBase where
  (SomeQuotePerBase sx x) == (SomeQuotePerBase sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show SomeQuotePerBase

unQu :: Qu a lcsu n -> n
unQu (Unsafe.Qu x) = x
