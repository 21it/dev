{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Metro
  ( Money (..),
    SomeMoney (..),
    MoneyBase',
    MoneyBaseAmt (..),
    MoneyQuote',
    MoneyQuoteAmt (..),
    QuotePerBase (..),
    SomeQuotePerBase (..),
    QuotePerBase',
    quotePerBaseAmt,
    roundMoney,
    roundQuotePerBase,
    moneyBaseBuy,
    moneyBaseSell,
    moneyQuoteBuy,
    moneyQuoteSell,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External hiding ((%))
import BitfinexClient.Util
import qualified Data.Aeson as A
import Data.Metrology.Poly
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
-- Money sugar
--

newtype
  Money
    (crel :: CurrencyRelation)
    (act :: ExchangeAction) = Money
  { unMoney ::
      MkQu_DLN (MoneyDim crel) LCSU' (Ratio Natural)
  }
  deriving stock
    ( Eq,
      Ord
    )

instance
  forall crel act unit.
  ( SingI crel,
    SingI act,
    Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  Prelude.Show (Money crel act)
  where
  show (Money x) =
    ( case sing :: Sing crel of
        SBase -> showIn x MoneyBaseAmt
        SQuote -> showIn x MoneyQuoteAmt
    )
      <> " "
      <> show (fromSing (sing :: Sing act))

instance
  ( SingI crel
  ) =>
  TryFrom (Ratio Natural) (Money crel act)
  where
  tryFrom =
    tryFrom @Rational `composeTryLhs` from

instance
  ( SingI crel
  ) =>
  From (Money crel act) (Ratio Natural)
  where
  from =
    unMkMoney

instance
  ( SingI crel
  ) =>
  TryFrom Rational (Money crel act)
  where
  tryFrom raw = do
    amt <- roundMoney raw
    if from amt == raw
      then pure amt
      else Left $ TryFromException raw Nothing

instance
  ( SingI crel
  ) =>
  From (Money crel act) Rational
  where
  from =
    via @(Ratio Natural)

deriving via
  Rational
  instance
    ( SingI crel,
      Typeable crel,
      Typeable act
    ) =>
    PersistFieldSql (Money crel act)

instance
  ( SingI crel,
    Typeable crel,
    Typeable act
  ) =>
  PersistField (Money crel act)
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
        showType @(Money crel act)
          <> " PersistValue is invalid "
          <> show raw

instance
  ( SingI crel,
    Typeable crel,
    Typeable act
  ) =>
  FromJSON (Money crel act)
  where
  parseJSON = A.withText
    (showType @(Money crel act))
    $ \x0 -> do
      case tryReadViaRatio @(Ratio Natural) x0 of
        Left x -> fail $ show x
        Right x -> pure x

--
-- SomeMoney sugar
--

data SomeMoney crel
  = forall act.
    ( SingI act,
      Show (Money crel act)
    ) =>
    SomeMoney
      (Sing act)
      (Money crel act)

instance Eq (SomeMoney crel) where
  (SomeMoney sx x) == (SomeMoney sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoney crel)

instance
  ( SingI crel,
    Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  TryFrom Rational (SomeMoney crel)
  where
  tryFrom raw
    | raw > 0 && rounded == raw =
      Right $
        SomeMoney (sing :: Sing 'Buy) $
          mkMoney absolute
    | raw < 0 && rounded == raw =
      Right $
        SomeMoney (sing :: Sing 'Sell) $
          mkMoney absolute
    | otherwise =
      Left $
        TryFromException raw Nothing
    where
      rounded = roundMoney' raw
      absolute = absRat raw

mkMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Ratio Natural ->
  Money crel act
mkMoney x =
  case sing :: Sing crel of
    SBase -> Money $ quOf x MoneyBaseAmt
    SQuote -> Money $ quOf x MoneyQuoteAmt

unMkMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Money crel act ->
  Ratio Natural
unMkMoney (Money x) =
  case sing :: Sing crel of
    SBase -> x # MoneyBaseAmt
    SQuote -> x # MoneyQuoteAmt

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
  show (QuotePerBase x) =
    showIn x quotePerBaseAmt
      <> " "
      <> show (fromSing (sing :: Sing act))

instance TryFrom (Ratio Natural) (QuotePerBase act) where
  tryFrom x
    | x > 0 = Right . QuotePerBase $ x % quotePerBaseAmt
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

roundMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Rational ->
  Either
    (TryFromException Rational (Money crel act))
    (Money crel act)
roundMoney raw =
  if raw >= 0 && rounded >= 0
    then
      bimap
        ( withTarget @(Money crel act)
            . withSource raw
        )
        mkMoney
        $ tryFrom @Rational @(Ratio Natural) rounded
    else
      Left $
        TryFromException raw Nothing
  where
    rounded =
      roundMoney' raw

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

roundMoney' :: Rational -> Rational
roundMoney' =
  dpRound 8

roundQuotePerBase' :: Rational -> Rational
roundQuotePerBase' =
  sdRound 5
    . dpRound 8

instance
  ( SingI act
  ) =>
  ToRequestParam (Money 'Base act)
  where
  toTextParam amt =
    case sing :: Sing act of
      SBuy -> toTextParam $ success amt
      SSell -> toTextParam $ (-1) * success amt
    where
      success :: Money 'Base act -> Rational
      success = abs . from . unMkMoney

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
            @(Money crel act)
            raw of
            Left e -> fail $ show e
            Right x -> do
              expr <- [e|mkMoney $(TH.lift $ unMkMoney x)|]
              case (sing :: Sing crel, sing :: Sing act) of
                (SBase, SBuy) -> do
                  TH.SigE expr
                    <$> [t|Money 'Base 'Buy|]
                (SBase, SSell) -> do
                  TH.SigE expr
                    <$> [t|Money 'Base 'Sell|]
                (SQuote, SBuy) -> do
                  TH.SigE expr
                    <$> [t|Money 'Quote 'Buy|]
                (SQuote, SSell) -> do
                  TH.SigE expr
                    <$> [t|Money 'Quote 'Sell|]
    }
  where
    failure :: Text -> a
    failure field =
      error $
        showType @(Money crel act)
          <> " "
          <> field
          <> " is not implemented"
