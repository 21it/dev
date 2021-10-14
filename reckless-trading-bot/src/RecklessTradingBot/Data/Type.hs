{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
  | ErrorTryFrom SomeException
  | ErrorSmartCon Text
  deriving stock (Show)

newtype OrderExternalId (a :: Bfx.ExchangeAction)
  = OrderExternalId Bfx.OrderId
  deriving newtype (Eq, Ord, Show)

deriving via
  Int64
  instance
    PersistFieldSql (OrderExternalId a)

instance PersistField (OrderExternalId a) where
  toPersistValue (OrderExternalId x) =
    case tryFrom x of
      Right id0 -> PersistInt64 id0
      Left {} ->
        error $
          "OrderExternalId Int64 overflow "
            <> show x
  fromPersistValue = \case
    x@(PersistInt64 id0) ->
      bimap (const $ failure x) OrderExternalId $ tryFrom id0
    x ->
      Left $ failure x
    where
      failure x = "OrderExternalId PersistValue is invalid " <> show x

instance From Bfx.OrderId (OrderExternalId a)

instance From (OrderExternalId a) Bfx.OrderId

data OrderStatus
  = OrderNew
  | OrderActive
  | OrderExecuted
  | OrderCancelled
  deriving stock (Eq, Ord, Show, Read)

instance From Bfx.OrderStatus OrderStatus where
  --
  -- TODO : verify this
  --
  from = \case
    Bfx.Active -> OrderActive
    Bfx.Executed -> OrderExecuted
    Bfx.PartiallyFilled -> OrderActive
    Bfx.InsufficientMargin -> OrderActive
    Bfx.Canceled -> OrderCancelled
    Bfx.PostOnlyCanceled -> OrderCancelled
    Bfx.RsnDust -> OrderActive
    Bfx.RsnPause -> OrderActive

derivePersistField "OrderStatus"
