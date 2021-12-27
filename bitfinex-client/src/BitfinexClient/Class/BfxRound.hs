{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.BfxRound
  ( Rounded,
    BfxRound,
    bfxRound,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Import.External
import qualified Witch

newtype Rounded a
  = Rounded a
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

-- | Trivial instances
instance From (Rounded a) a

instance
  ( BfxRound a,
    Eq a
  ) =>
  TryFrom a (Rounded a)
  where
  tryFrom raw =
    case bfxRound raw of
      Right res@(Rounded x) | x == raw -> pure res
      _ -> Left $ TryFromException raw Nothing

-- | 'Ratio Natural' instances
instance
  ( From (Rounded a) a,
    From a (Ratio Natural),
    'False ~ (Rounded a == a),
    'False ~ (a == Ratio Natural)
  ) =>
  From (Rounded a) (Ratio Natural)
  where
  from = via @a

instance
  TryFrom
    (Ratio Natural)
    (Rounded (MoneyAmt dim act))
  where
  tryFrom =
    tryFrom @(MoneyAmt dim act)
      `composeTryLhs` from

instance
  TryFrom
    (Ratio Natural)
    (Rounded (QuotePerBase act))
  where
  tryFrom =
    tryVia @(QuotePerBase act)

-- | 'Rational' instances
instance
  ( From (Rounded a) a,
    From a Rational,
    'False ~ (Rounded a == a),
    'False ~ (a == Rational)
  ) =>
  From (Rounded a) Rational
  where
  from = via @a

instance
  ( TryFrom Rational a,
    TryFrom a (Rounded a),
    'False ~ (Rational == a),
    'False ~ (a == Rounded a)
  ) =>
  TryFrom Rational (Rounded a)
  where
  tryFrom =
    tryVia @a

-- | Private class to allow polymorphism
class
  ( From a Rational,
    TryFrom Rational a
  ) =>
  BfxRound a
  where
  bfxRound ::
    a ->
    Either
      (TryFromException Rational (Rounded a))
      (Rounded a)

instance BfxRound (QuotePerBase act) where
  bfxRound =
    bimap withTarget Rounded
      . tryFrom
      . sdRound 5
      . dpRound 8
      . from

instance BfxRound (MoneyAmt dim act) where
  bfxRound =
    bimap withTarget Rounded
      . tryFrom
      . dpRound 8
      . from

instance
  ( SingI act
  ) =>
  ToRequestParam (Rounded (MoneyBase act))
  where
  toTextParam amt =
    case sing :: Sing act of
      SBuy -> toTextParam $ success amt
      SSell -> toTextParam $ (-1) * success amt
    where
      success :: Rounded (MoneyAmt dim act) -> Rational
      success = abs . from

instance ToRequestParam (Rounded (QuotePerBase act)) where
  toTextParam =
    toTextParam
      . from @(Rounded (QuotePerBase act)) @(Ratio Natural)
