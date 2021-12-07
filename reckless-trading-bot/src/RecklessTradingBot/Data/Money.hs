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
  = QuotePerBase Bfx.QuotePerBase
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

instance From Bfx.QuotePerBase (QuotePerBase a)

instance From (QuotePerBase a) Bfx.QuotePerBase

instance From (QuotePerBase a) Rational where
  from = via @Bfx.QuotePerBase

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
        from @Bfx.QuotePerBase `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "QuotePerBase PersistValue is invalid " <> show x

newtype MoneyBase
  = MoneyBase Bfx.MoneyBase
  deriving newtype (Eq, Ord, Show)

instance From (Ratio Natural) MoneyBase where
  from = via @Bfx.MoneyBase

instance From Bfx.MoneyBase MoneyBase

instance From MoneyBase Bfx.MoneyBase

instance From MoneyBase Rational where
  from = via @Bfx.MoneyBase

deriving via
  Rational
  instance
    PersistFieldSql MoneyBase

instance PersistField MoneyBase where
  toPersistValue =
    PersistRational . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @Bfx.MoneyBase `composeTryRhs` tryFrom $ x
    x ->
      Left $
        "MoneyBase PersistValue is invalid "
          <> show x

newtype MoneyQuote
  = MoneyQuote Bfx.MoneyQuote
  deriving newtype (Eq, Ord, Show)

instance From (Ratio Natural) MoneyQuote where
  from = via @Bfx.MoneyQuote

instance From Bfx.MoneyQuote MoneyQuote

instance From MoneyQuote Bfx.MoneyQuote

instance From MoneyQuote Rational where
  from = via @Bfx.MoneyQuote

deriving via
  Rational
  instance
    PersistFieldSql MoneyQuote

instance PersistField MoneyQuote where
  toPersistValue =
    PersistRational . from
  fromPersistValue = \case
    PersistRational x ->
      first show $
        from @Bfx.MoneyQuote `composeTryRhs` tryFrom $ x
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
