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

newtype QuotePerBase (act :: Bfx.ExchangeAction)
  = QuotePerBase (Bfx.QuotePerBase act)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From (Bfx.QuotePerBase act) (QuotePerBase act)

instance From (QuotePerBase act) (Bfx.QuotePerBase act)

instance From (QuotePerBase act) Rational where
  from = via @(Bfx.QuotePerBase act)

deriving via
  Rational
  instance
    (Typeable act) =>
    PersistFieldSql (QuotePerBase act)

instance (Typeable act) => PersistField (QuotePerBase act) where
  toPersistValue =
    PersistRational
      . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @(Bfx.QuotePerBase act) `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "QuotePerBase PersistValue is invalid " <> show x

newtype MoneyBase act
  = MoneyBase (Bfx.MoneyBase act)
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From (Ratio Natural) (MoneyBase act) where
  from = via @(Bfx.MoneyBase act)

instance From (Bfx.MoneyBase act) (MoneyBase act)

instance From (MoneyBase act) (Bfx.MoneyBase act)

instance (SingI act) => From (MoneyBase act) Rational where
  from = via @(Bfx.MoneyBase act)

deriving via
  Rational
  instance
    ( Typeable act,
      SingI act
    ) =>
    PersistFieldSql (MoneyBase act)

instance
  ( Typeable act,
    SingI act
  ) =>
  PersistField (MoneyBase act)
  where
  toPersistValue =
    PersistRational . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @(Bfx.MoneyBase act) `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "MoneyBase PersistValue is invalid "
          <> show x

newtype MoneyQuote act
  = MoneyQuote (Bfx.MoneyQuote act)
  deriving newtype (Eq, Ord, Show)

instance From (Ratio Natural) (MoneyQuote act) where
  from = via @(Bfx.MoneyQuote act)

instance From (Bfx.MoneyQuote act) (MoneyQuote act)

instance From (MoneyQuote act) (Bfx.MoneyQuote act)

instance
  ( SingI act
  ) =>
  From (MoneyQuote act) Rational
  where
  from = via @(Bfx.MoneyQuote act)

deriving via
  Rational
  instance
    ( Typeable act,
      SingI act
    ) =>
    PersistFieldSql (MoneyQuote act)

instance
  ( Typeable act,
    SingI act
  ) =>
  PersistField (MoneyQuote act)
  where
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
