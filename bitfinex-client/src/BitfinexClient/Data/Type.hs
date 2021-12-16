{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Type
  ( -- * Orders
    -- $orders
    OrderId (..),
    OrderClientId (..),
    OrderGroupId (..),
    Order (..),
    SomeOrder (..),
    OrderFlag (..),
    OrderFlagAcc (..),
    unOrderFlag,
    unOrderFlagSet,
    OrderStatus (..),
    newOrderStatus,

    -- * Trading
    -- $trading
    FeeRate (..),
    RebateRate (..),
    ProfitRate (..),
    CurrencyCode (..),
    newCurrencyCode,
    CurrencyPair,
    currencyPairCon,
    currencyPairBase,
    currencyPairQuote,
    newCurrencyPair,
    CurrencyPairConf (..),

    -- * Misc
    -- $misc
    Error (..),
    tryErrorE,
    tryErrorT,
    tryFromE,
    tryFromT,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Import.External
import Data.Aeson (withObject, (.:))
import qualified Data.Text as T
import Language.Haskell.TH.Syntax as TH (Lift)
import qualified Network.HTTP.Client as Web

-- $orders
-- Order data received from Bitfinex
-- and types related to orders.

newtype OrderId
  = OrderId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderId

instance From OrderId Natural

instance TryFrom Int64 OrderId where
  tryFrom =
    from @Natural `composeTryRhs` tryFrom

instance TryFrom OrderId Int64 where
  tryFrom =
    tryFrom @Natural `composeTryLhs` from

newtype OrderClientId
  = OrderClientId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderClientId

instance From OrderClientId Natural

newtype OrderGroupId
  = OrderGroupId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderGroupId

instance From OrderGroupId Natural

data Order (act :: ExchangeAction) (loc :: Location) = Order
  { orderId :: OrderId,
    orderGroupId :: Maybe OrderGroupId,
    -- | Field might be auto-generated by Bitfinex in case where
    -- it was not provided through 'BitfinexClient.Data.SubmitOrder.Options'.
    orderClientId :: Maybe OrderClientId,
    orderAmount :: MoneyBase act,
    orderSymbol :: CurrencyPair,
    orderRate :: QuotePerBase act,
    orderStatus :: OrderStatus
  }
  deriving stock
    ( Eq,
      Show,
      Generic
    )

data SomeOrder (loc :: Location)
  = forall (act :: ExchangeAction).
    ( Show (Order act loc),
      SingI act
    ) =>
    SomeOrder
      (Sing act)
      (Order act loc)

deriving stock instance Show (SomeOrder loc)

instance Eq (SomeOrder loc) where
  (SomeOrder sx x) == (SomeOrder sy y) =
    case eqExchangeAction sx sy of
      Just Refl -> x == y
      Nothing -> False

data OrderFlag
  = Hidden
  | Close
  | ReduceOnly
  | PostOnly
  | Oco
  | NoVarRates
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      Enum,
      Bounded
    )

newtype OrderFlagAcc
  = OrderFlagAcc Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

unOrderFlag :: OrderFlag -> OrderFlagAcc
unOrderFlag =
  OrderFlagAcc . \case
    Hidden -> 64
    Close -> 512
    ReduceOnly -> 1024
    PostOnly -> 4096
    Oco -> 16384
    NoVarRates -> 524288

unOrderFlagSet :: Set OrderFlag -> OrderFlagAcc
unOrderFlagSet =
  foldr (\x acc -> acc + unOrderFlag x) $ OrderFlagAcc 0

data OrderStatus
  = Active
  | Executed
  | PartiallyFilled
  | InsufficientMargin
  | Canceled
  | PostOnlyCanceled
  | RsnDust
  | RsnPause
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      Enum,
      Bounded
    )

newOrderStatus ::
  Text ->
  Either
    (TryFromException Text OrderStatus)
    OrderStatus
newOrderStatus = \case
  "ACTIVE" -> Right Active
  x | "EXECUTED" `T.isPrefixOf` x -> Right Executed
  x | "PARTIALLY FILLED" `T.isPrefixOf` x -> Right PartiallyFilled
  x | "INSUFFICIENT MARGIN" `T.isPrefixOf` x -> Right InsufficientMargin
  "CANCELED" -> Right Canceled
  "POSTONLY CANCELED" -> Right PostOnlyCanceled
  "RSN_DUST" -> Right RsnDust
  "RSN_PAUSE" -> Right RsnPause
  x -> Left $ TryFromException x Nothing

-- $trading
-- Data related to trading and money.

newtype
  FeeRate
    (mrel :: MarketRelation)
    (crel :: CurrencyRelation) = FeeRate
  { unFeeRate :: Ratio Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

instance From (FeeRate mrel crel) (Ratio Natural)

instance TryFrom (Ratio Natural) (FeeRate mrel crel) where
  tryFrom x
    | x < 1 = Right $ FeeRate x
    | otherwise = Left $ TryFromException x Nothing

instance From (FeeRate mrel crel) Rational where
  from = via @(Ratio Natural)

instance TryFrom Rational (FeeRate mrel crel) where
  tryFrom = tryVia @(Ratio Natural)

deriving via
  Rational
  instance
    PersistFieldSql (FeeRate mrel crel)

instance PersistField (FeeRate mrel crel) where
  toPersistValue =
    PersistRational . from
  fromPersistValue raw =
    case raw of
      PersistRational x ->
        first (const failure) $ tryFrom x
      _ ->
        Left failure
    where
      failure =
        "FeeRate PersistValue is invalid " <> show raw

newtype RebateRate (mrel :: MarketRelation)
  = RebateRate Rational
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance From (RebateRate mrel) Rational

instance From Rational (RebateRate mrel)

newtype ProfitRate = ProfitRate
  { unProfitRate :: Ratio Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

instance TryFrom (Ratio Natural) ProfitRate where
  tryFrom x
    | x > 0 = Right $ ProfitRate x
    | otherwise = Left $ TryFromException x Nothing

instance From ProfitRate (Ratio Natural)

instance TryFrom Rational ProfitRate where
  tryFrom =
    tryVia @(Ratio Natural)

instance From ProfitRate Rational where
  from =
    via @(Ratio Natural)

newtype CurrencyCode (crel :: CurrencyRelation) = CurrencyCode
  { unCurrencyCode :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      --
      -- TODO : maybe we want to handle it as
      -- case-insensitive Text
      --
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

deriving via
  Text
  instance
    PersistFieldSql (CurrencyCode crel)

instance PersistField (CurrencyCode crel) where
  toPersistValue =
    PersistText . coerce
  fromPersistValue raw =
    case raw of
      PersistText x ->
        first (const failure) $ newCurrencyCode x
      _ ->
        Left failure
    where
      failure =
        "CurrencyCode PersistValue is invalid " <> show raw

newCurrencyCode ::
  forall crel.
  Text ->
  Either
    (TryFromException Text (CurrencyCode crel))
    (CurrencyCode crel)
newCurrencyCode raw =
  case T.strip raw of
    x | length x == 3 -> Right . CurrencyCode $ T.toUpper x
    _ -> Left $ TryFromException raw Nothing

data CurrencyPair = CurrencyPair
  { currencyPairBase :: CurrencyCode 'Base,
    currencyPairQuote :: CurrencyCode 'Quote
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      TH.Lift
    )

currencyPairCon ::
  CurrencyCode 'Base ->
  CurrencyCode 'Quote ->
  Either
    ( TryFromException
        ( CurrencyCode 'Base,
          CurrencyCode 'Quote
        )
        CurrencyPair
    )
    CurrencyPair
currencyPairCon base quote =
  if unCurrencyCode base == unCurrencyCode quote
    then
      Left $
        TryFromException (base, quote) Nothing
    else
      Right $
        CurrencyPair base quote

instance FromJSON CurrencyPair where
  parseJSON = withObject "CurrencyPair" $ \x0 -> do
    base <- x0 .: "base"
    quote <- x0 .: "quote"
    case currencyPairCon base quote of
      Left x -> fail $ show x
      Right x -> pure x

instance ToRequestParam CurrencyPair where
  toTextParam x =
    "t"
      <> (coerce $ currencyPairBase x :: Text)
      <> (coerce $ currencyPairQuote x :: Text)

newCurrencyPair ::
  Text ->
  Either (TryFromException Text CurrencyPair) CurrencyPair
newCurrencyPair raw
  | (length nakedRaw == 7) && (prefix == "t") = do
    let (base0, quote0) = T.splitAt 3 postfix
    base <- withFirst $ newCurrencyCode base0
    quote <- withFirst $ newCurrencyCode quote0
    withFirst $ currencyPairCon base quote
  | length nakedRaw == 6 = do
    let (base0, quote0) = T.splitAt 3 nakedRaw
    base <- withFirst $ newCurrencyCode base0
    quote <- withFirst $ newCurrencyCode quote0
    withFirst $ currencyPairCon base quote
  | otherwise =
    Left $ TryFromException nakedRaw Nothing
  where
    nakedRaw = T.strip raw
    (prefix, postfix) = T.splitAt 1 nakedRaw
    withFirst ::
      Either (TryFromException source target) a ->
      Either (TryFromException Text CurrencyPair) a
    withFirst =
      first $
        withTarget @CurrencyPair . withSource raw

data CurrencyPairConf = CurrencyPairConf
  { currencyPairPrecision :: Natural,
    currencyPairInitMargin :: Ratio Natural,
    currencyPairMinMargin :: Ratio Natural,
    currencyPairMaxOrderAmt :: MoneyBase 'Buy,
    currencyPairMinOrderAmt :: MoneyBase 'Buy
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

-- $misc
-- General utility data used elsewhere.

data Error
  = ErrorWebException HttpException
  | ErrorWebPub Web.Request (Web.Response ByteString)
  | ErrorWebPrv ByteString Web.Request (Web.Response ByteString)
  | ErrorParser Web.Request (Web.Response ByteString) Text
  | ErrorMath Text
  | ErrorTryFrom SomeException
  | ErrorMissingOrder OrderId
  | ErrorUnverifiedOrder (SomeOrder 'Local) (SomeOrder 'Remote)
  | ErrorOrderState (SomeOrder 'Remote)
  deriving stock
    --
    -- TODO : implement Eq/Ord?
    --
    ( Show,
      Generic
    )

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
    TryFrom source target
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
    Monad m
  ) =>
  source ->
  ExceptT Error m target
tryFromT =
  except . tryFromE
