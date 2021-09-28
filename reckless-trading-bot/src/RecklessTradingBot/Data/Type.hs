{-# LANGUAGE TemplateHaskell #-}

module RecklessTradingBot.Data.Type
  ( LogFormat (..),
    Error (..),
    OrderExternalId (..),
    OrderStatus (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import.External

data LogFormat
  = Bracket
  | Json
  deriving stock (Eq, Ord, Show, Read)

data Error
  = ErrorBfx Bfx.Error
  | ErrorSmartCon Text
  deriving stock (Show)

newtype OrderExternalId (a :: Bfx.ExchangeAction)
  = OrderExternalId Natural
  deriving newtype (Eq, Ord, Show)

deriving via
  Int64
  instance
    PersistFieldSql (OrderExternalId a)

instance PersistField (OrderExternalId a) where
  toPersistValue (OrderExternalId x) =
    case tryFrom x of
      Right id0 -> PersistInt64 id0
      Left {} -> error $ "OrderExternalId Int64 overflow " <> show x
  fromPersistValue = \case
    x@(PersistInt64 id0) ->
      bimap (const $ failure x) OrderExternalId $ tryFrom id0
    x ->
      Left $ failure x
    where
      failure x = "OrderExternalId PersistValue is invalid " <> show x

data OrderStatus
  = OrderNew
  | OrderActive
  | OrderExecuted
  | OrderCancelled
  deriving stock (Eq, Ord, Show, Read)

derivePersistField "OrderStatus"
