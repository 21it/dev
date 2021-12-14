{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Money
  ( CurrencyCode (..),
    QuotePerBase (..),
    MoneyBase (..),
    MoneyQuote (..),
    FeeRate (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import.External

newtype CurrencyCode (a :: Bfx.CurrencyRelation)
  = CurrencyCode (Bfx.CurrencyCode a)
  deriving newtype (Eq, Ord, Show, IsString)

instance From (Bfx.CurrencyCode a) (CurrencyCode a)

instance From (CurrencyCode a) (Bfx.CurrencyCode a)

deriving via
  Text
  instance
    PersistFieldSql (CurrencyCode a)

instance PersistField (CurrencyCode a) where
  toPersistValue =
    PersistText . coerce
  fromPersistValue = \case
    PersistText x ->
      Right . CurrencyCode $
        Bfx.CurrencyCode x
    x ->
      Left $
        "CurrencyCode PersistValue is invalid "
          <> show x

newtype QuotePerBase (a :: Bfx.ExchangeAction)
  = QuotePerBase (Bfx.QuotePerBase a)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From (Bfx.QuotePerBase a) (QuotePerBase a)

instance From (QuotePerBase a) (Bfx.QuotePerBase a)

instance From (QuotePerBase a) Rational where
  from = via @(Bfx.QuotePerBase a)

deriving via
  Rational
  instance
    (Typeable a) =>
    PersistFieldSql (QuotePerBase a)

instance (Typeable a) => PersistField (QuotePerBase a) where
  toPersistValue =
    PersistRational
      . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @(Bfx.QuotePerBase a) `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "QuotePerBase PersistValue is invalid " <> show x

newtype MoneyBase a
  = MoneyBase (Bfx.MoneyBase a)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From (Ratio Natural) (MoneyBase a) where
  from = via @(Bfx.MoneyBase a)

instance From (Bfx.MoneyBase a) (MoneyBase a)

instance From (MoneyBase a) (Bfx.MoneyBase a)

instance From (MoneyBase a) Rational where
  from = via @(Bfx.MoneyBase a)

deriving via
  Rational
  instance
    (Typeable a) =>
    PersistFieldSql (MoneyBase a)

instance (Typeable a) => PersistField (MoneyBase a) where
  toPersistValue =
    PersistRational . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @(Bfx.MoneyBase a) `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "MoneyBase PersistValue is invalid "
          <> show x

newtype MoneyQuote a
  = MoneyQuote (Bfx.MoneyQuote a)
  deriving newtype (Eq, Ord, Show)

instance From (Ratio Natural) (MoneyQuote a) where
  from = via @(Bfx.MoneyQuote a)

instance From (Bfx.MoneyQuote act) (MoneyQuote act)

instance From (MoneyQuote act) (Bfx.MoneyQuote act)

instance From (MoneyQuote act) Rational where
  from = via @(Bfx.MoneyQuote act)

deriving via
  Rational
  instance
    (Typeable act) => PersistFieldSql (MoneyQuote act)

instance (Typeable act) => PersistField (MoneyQuote act) where
  toPersistValue =
    PersistRational . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @(Bfx.MoneyQuote act) `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "MoneyQuote PersistValue is invalid "
          <> show x

newtype
  FeeRate
    (a :: Bfx.MarketRelation)
    (b :: Bfx.CurrencyRelation)
  = FeeRate (Bfx.FeeRate a b)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From (FeeRate a b) (Bfx.FeeRate a b)

instance From (Bfx.FeeRate a b) (FeeRate a b)

instance From (FeeRate a b) Rational where
  from = via @(Bfx.FeeRate a b)

instance TryFrom Rational (FeeRate a b) where
  tryFrom =
    from @(Bfx.FeeRate a b)
      `composeTryRhs` tryFrom

deriving via
  Rational
  instance
    PersistFieldSql (FeeRate a b)

instance PersistField (FeeRate a b) where
  toPersistValue = PersistRational . from
  fromPersistValue = \case
    x0@(PersistRational x) ->
      first (const $ failure x0) $ tryFrom x
    x0 ->
      Left $ failure x0
    where
      failure :: (Show c) => c -> Text
      failure =
        ("FeeRate PersistValue is invalid " <>) . show
