{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Type
  ( LogFormat (..),
    Error (..),
    OrderExternalId (..),
    OrderStatus (..),
    tryFromT,
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
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Enum,
      Bounded,
      Generic
    )

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

tryFromT ::
  forall source target m.
  ( Monad m,
    TryFrom source target,
    Show source,
    Typeable source,
    Typeable target
  ) =>
  source ->
  ExceptT Error m target
tryFromT =
  except
    . first (ErrorTryFrom . SomeException)
    . tryFrom
