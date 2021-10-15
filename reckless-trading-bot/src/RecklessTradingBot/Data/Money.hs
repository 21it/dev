{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Money
  ( CurrencyCode (..),
    ExchangeRate (..),
    MoneyAmount (..),
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

newtype ExchangeRate (a :: Bfx.ExchangeAction)
  = ExchangeRate Bfx.ExchangeRate
  deriving newtype (Eq, Ord, Show, Num)

instance From Bfx.ExchangeRate (ExchangeRate a)

instance From (ExchangeRate a) Bfx.ExchangeRate

deriving via
  Rational
  instance
    PersistFieldSql (ExchangeRate a)

instance PersistField (ExchangeRate a) where
  toPersistValue =
    PersistRational
      . Bfx.fromRatio
      . Bfx.unPosRat
      . coerce
  fromPersistValue = \case
    PersistRational x0 ->
      case Bfx.newExchangeRate x0 of
        Left e -> Left $ show e
        Right x1 -> Right $ ExchangeRate x1
    x ->
      Left $
        "ExchangeRate PersistValue is invalid "
          <> show x

newtype MoneyAmount (a :: Bfx.CurrencyRelation)
  = MoneyAmount Bfx.MoneyAmount
  deriving newtype (Eq, Ord, Show, Num, FromJSON)

instance From Bfx.MoneyAmount (MoneyAmount a)

instance From Bfx.PosRat (MoneyAmount a) where
  from = via @Bfx.MoneyAmount

instance From (MoneyAmount a) Bfx.MoneyAmount

instance From (MoneyAmount a) Bfx.PosRat where
  from = via @Bfx.MoneyAmount

deriving via
  Rational
  instance
    PersistFieldSql (MoneyAmount a)

instance PersistField (MoneyAmount a) where
  toPersistValue =
    PersistRational
      . Bfx.fromRatio
      . Bfx.unPosRat
      . coerce
  fromPersistValue = \case
    PersistRational x0 ->
      case Bfx.newMoneyAmount x0 of
        Left e -> Left $ show e
        Right x1 -> Right $ MoneyAmount x1
    x ->
      Left $
        "MoneyAmount PersistValue is invalid "
          <> show x

newtype FeeRate (a :: Bfx.CurrencyRelation)
  = FeeRate Bfx.PosRat
  deriving newtype (Eq, Ord, Show, Num)

deriving via
  Rational
  instance
    PersistFieldSql (FeeRate a)

instance PersistField (FeeRate a) where
  toPersistValue =
    PersistRational
      . Bfx.fromRatio
      . Bfx.unPosRat
      . coerce
  fromPersistValue = \case
    PersistRational x0 ->
      case Bfx.newPosRat x0 of
        Left e -> Left $ show e
        Right x1 -> Right $ FeeRate x1
    x ->
      Left $
        "FeeRate PersistValue is invalid "
          <> show x
