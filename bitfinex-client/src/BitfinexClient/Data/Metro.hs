{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
    roundMoneyAmt,
    roundQuotePerBase,
    moneyBaseBuy,
    moneyBaseSell,
    moneyQuoteBuy,
    moneyQuoteSell,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External hiding (exp, (%))
import BitfinexClient.Util
import qualified Data.Aeson as A
import Data.Metrology.Poly as Metro
--
-- TODO : somehow remove unsafe
--
import qualified Data.Metrology.Unsafe as Unsafe
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch
import qualified Prelude

data MoneyDim (crel :: CurrencyRelation)

instance Dimension (MoneyDim 'Base)

instance Dimension (MoneyDim 'Quote)

data MoneyBaseAmt = MoneyBaseAmt

instance Unit MoneyBaseAmt where
  type BaseUnit MoneyBaseAmt = Canonical
  type DimOfUnit MoneyBaseAmt = MoneyDim 'Base

instance Show MoneyBaseAmt where
  show = const "MoneyBaseAmt"

data MoneyQuoteAmt = MoneyQuoteAmt

instance Unit MoneyQuoteAmt where
  type BaseUnit MoneyQuoteAmt = Canonical
  type DimOfUnit MoneyQuoteAmt = MoneyDim 'Quote

instance Show MoneyQuoteAmt where
  show = const "MoneyQuoteAmt"

type LCSU' =
  MkLCSU
    '[ (MoneyDim 'Base, MoneyBaseAmt),
       (MoneyDim 'Quote, MoneyQuoteAmt)
     ]

type MoneyBase' =
  MkQu_DLN (MoneyDim 'Base) LCSU' (Ratio Natural)

type MoneyQuote' =
  MkQu_DLN (MoneyDim 'Quote) LCSU' (Ratio Natural)

--
-- MoneyAmt sugar
--

newtype
  MoneyAmt
    (crel :: CurrencyRelation)
    (act :: ExchangeAction) = MoneyAmt
  { unMoneyAmt ::
      MkQu_DLN (MoneyDim crel) LCSU' (Ratio Natural)
  }
  deriving stock
    ( Eq,
      Ord
    )

instance
  ( Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  Prelude.Show (MoneyAmt crel act)
  where
  show x =
    show (unQu $ unMoneyAmt x)
      <> " "
      <> showType @unit

type MoneyBase = MoneyAmt 'Base

type MoneyQuote = MoneyAmt 'Quote

instance TryFrom (Ratio Natural) (MoneyAmt crel act) where
  tryFrom =
    tryFrom @Rational `composeTryLhs` from

instance From (MoneyAmt crel act) (Ratio Natural) where
  from =
    unQu . unMoneyAmt

instance TryFrom Rational (MoneyAmt crel act) where
  tryFrom raw = do
    amt <- roundMoneyAmt raw
    if from amt == raw
      then pure amt
      else Left $ TryFromException raw Nothing

instance From (MoneyAmt crel act) Rational where
  from =
    via @(Ratio Natural)

deriving via
  Rational
  instance
    ( Typeable crel,
      Typeable act
    ) =>
    PersistFieldSql (MoneyAmt crel act)

instance
  ( Typeable crel,
    Typeable act
  ) =>
  PersistField (MoneyAmt crel act)
  where
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
        showType @(MoneyAmt crel act)
          <> " PersistValue is invalid "
          <> show raw

instance
  ( Typeable crel,
    Typeable act
  ) =>
  FromJSON (MoneyAmt crel act)
  where
  parseJSON = A.withText
    (showType @(MoneyAmt crel act))
    $ \x0 -> do
      case tryReadViaRatio @(Ratio Natural) x0 of
        Left x -> fail $ show x
        Right x -> pure x

--
-- SomeMoneyAmt sugar
--

data SomeMoneyAmt crel
  = forall act.
    ( Show (MoneyAmt crel act),
      SingI act
    ) =>
    SomeMoneyAmt
      (Sing act)
      (MoneyAmt crel act)

type SomeMoneyBase = SomeMoneyAmt 'Base

type SomeMoneyQuote = SomeMoneyAmt 'Quote

instance Eq (SomeMoneyAmt crel) where
  (SomeMoneyAmt sx x) == (SomeMoneyAmt sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoneyAmt crel)

instance
  ( Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  TryFrom Rational (SomeMoneyAmt crel)
  where
  tryFrom raw
    | raw > 0 && rounded > 0 =
      Right
        . SomeMoneyAmt (sing :: Sing 'Buy)
        . MoneyAmt
        . Unsafe.Qu
        $ absRat raw
    | raw < 0 && rounded < 0 =
      Right
        . SomeMoneyAmt (sing :: Sing 'Sell)
        . MoneyAmt
        . Unsafe.Qu
        $ absRat raw
    | otherwise =
      Left $
        TryFromException raw Nothing
    where
      rounded = roundMoneyAmt' raw

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

instance (SingI act) => Prelude.Show (QuotePerBase act) where
  show x =
    show (unQuotePerBase x # quotePerBaseAmt)
      <> " "
      <> show quotePerBaseAmt
      <> " "
      <> show (fromSing (sing :: Sing act))

instance TryFrom (Ratio Natural) (QuotePerBase act) where
  tryFrom x
    | x > 0 = Right . QuotePerBase $ x Metro.% quotePerBaseAmt
    | otherwise = Left $ TryFromException x Nothing

instance From (QuotePerBase act) (Ratio Natural) where
  from =
    (# quotePerBaseAmt) . unQuotePerBase

instance TryFrom Rational (QuotePerBase act) where
  tryFrom raw = do
    rate <- roundQuotePerBase raw
    if from rate == raw
      then pure rate
      else Left $ TryFromException raw Nothing

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

roundMoneyAmt ::
  forall crel act.
  Rational ->
  Either
    (TryFromException Rational (MoneyAmt crel act))
    (MoneyAmt crel act)
roundMoneyAmt raw =
  if raw >= 0 && rounded >= 0
    then
      bimap
        ( withTarget @(MoneyAmt crel act)
            . withSource raw
        )
        ( MoneyAmt
            . Unsafe.Qu
        )
        $ tryFrom @Rational @(Ratio Natural) rounded
    else
      Left $
        TryFromException raw Nothing
  where
    rounded =
      roundMoneyAmt' raw

roundQuotePerBase ::
  forall act.
  Rational ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
roundQuotePerBase raw =
  if raw > 0 && rounded > 0
    then
      bimap
        ( withTarget @(QuotePerBase act)
            . withSource raw
        )
        ( QuotePerBase
            . (% quotePerBaseAmt)
        )
        $ tryFrom @Rational @(Ratio Natural) rounded
    else
      Left $
        TryFromException raw Nothing
  where
    rounded =
      roundQuotePerBase' raw

roundMoneyAmt' :: Rational -> Rational
roundMoneyAmt' =
  dpRound 8

roundQuotePerBase' :: Rational -> Rational
roundQuotePerBase' =
  sdRound 5
    . dpRound 8

instance (SingI act) => ToRequestParam (MoneyBase act) where
  toTextParam amt =
    case sing :: Sing act of
      SBuy -> toTextParam $ success amt
      SSell -> toTextParam $ (-1) * success amt
    where
      success :: MoneyAmt crel act -> Rational
      success = abs . from . unQu . unMoneyAmt

instance ToRequestParam (QuotePerBase act) where
  toTextParam =
    toTextParam
      . from @(QuotePerBase act) @(Ratio Natural)

moneyBaseBuy :: QuasiQuoter
moneyBaseBuy =
  moneyQQ @'Base @'Buy

moneyBaseSell :: QuasiQuoter
moneyBaseSell =
  moneyQQ @'Base @'Sell

moneyQuoteBuy :: QuasiQuoter
moneyQuoteBuy =
  moneyQQ @'Quote @'Buy

moneyQuoteSell :: QuasiQuoter
moneyQuoteSell =
  moneyQQ @'Quote @'Sell

moneyQQ ::
  forall (crel :: CurrencyRelation) (act :: ExchangeAction).
  ( SingI crel,
    SingI act,
    Typeable crel,
    Typeable act
  ) =>
  QuasiQuoter
moneyQQ =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \raw ->
          case tryReadViaRatio
            @Rational
            @(MoneyAmt crel act)
            raw of
            Left e -> fail $ show e
            Right (MoneyAmt x) -> do
              --
              -- TODO : remove unsafe, use corresponding units
              --
              exp <-
                [e|
                  MoneyAmt $
                    Unsafe.Qu $(TH.lift $ unQu x)
                  |]
              case (sing :: Sing crel, sing :: Sing act) of
                (SBase, SBuy) -> do
                  TH.SigE exp
                    <$> [t|MoneyAmt 'Base 'Buy|]
                (SBase, SSell) -> do
                  TH.SigE exp
                    <$> [t|MoneyAmt 'Base 'Sell|]
                (SQuote, SBuy) -> do
                  TH.SigE exp
                    <$> [t|MoneyAmt 'Quote 'Buy|]
                (SQuote, SSell) -> do
                  TH.SigE exp
                    <$> [t|MoneyAmt 'Quote 'Sell|]
    }
  where
    failure :: Text -> a
    failure field =
      error $
        showType @(MoneyAmt crel act)
          <> " "
          <> field
          <> " is not implemented"
