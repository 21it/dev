{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Type
  ( LogFormat (..),
    TradeMode (..),
    Error (..),
    OrderExternalId (..),
    OrderStatus (..),
    newOrderStatus,
    tryErrorE,
    tryErrorT,
    tryFromE,
    tryFromT,
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import.External

data LogFormat
  = Bracket
  | Json
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
    )

data TradeMode
  = Speculate
  | BuyOnly
  | SellOnly
  | ObserveOnly
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
    )

instance FromJSON TradeMode

data Error
  = ErrorBfx Bfx.Error
  | ErrorTryFrom SomeException
  | ErrorRuntime Text
  deriving stock
    ( Show
    )

newtype OrderExternalId (act :: Bfx.ExchangeAction)
  = OrderExternalId Bfx.OrderId
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic
    )

deriving via
  Text
  instance
    PersistFieldSql (OrderExternalId act)

instance PersistField (OrderExternalId act) where
  toPersistValue (OrderExternalId x) =
    PersistText . show $ into @Natural x
  fromPersistValue = \case
    x@(PersistText id0) ->
      bimap (const $ failure x) OrderExternalId $
        readVia @Natural id0
    x ->
      Left $ failure x
    where
      failure x =
        "OrderExternalId PersistValue is invalid " <> show x

instance From Bfx.OrderId (OrderExternalId act)

instance From (OrderExternalId act) Bfx.OrderId

data OrderStatus
  = OrderNew
  | OrderActive
  | OrderExecuted
  | -- | Final statuses
    OrderCancelled
  | OrderCountered
  | OrderUnexpected
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Enum,
      Bounded,
      Generic
    )

newOrderStatus :: Bfx.OrderStatus -> OrderStatus
newOrderStatus = \case
  Bfx.Active -> OrderActive
  Bfx.Executed -> OrderExecuted
  Bfx.Cancelled -> OrderCancelled
  Bfx.PostOnlyCancelled -> OrderCancelled
  Bfx.PartiallyFilled -> OrderActive
  Bfx.InsufficientMargin -> OrderUnexpected
  Bfx.RsnDust -> OrderUnexpected
  Bfx.RsnPause -> OrderUnexpected

derivePersistField "OrderStatus"

tryErrorE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target
  ) =>
  Either (TryFromException source target) target ->
  Either Error target
tryErrorE =
  first $
    ErrorTryFrom . SomeException

tryErrorT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    Monad m
  ) =>
  Either (TryFromException source target) target ->
  ExceptT Error m target
tryErrorT =
  except . tryErrorE

tryFromE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    'False ~ (source == target)
  ) =>
  source ->
  Either Error target
tryFromE =
  tryErrorE . tryFrom

tryFromT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    Monad m,
    'False ~ (source == target)
  ) =>
  source ->
  ExceptT Error m target
tryFromT =
  except . tryFromE
