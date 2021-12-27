{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.BfxRound
  ( Rounded,
    BfxRound,
    bfxRound,
    bfxUnRound,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Import.External

newtype Rounded a = Rounded
  { bfxUnRound :: a
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

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
      success = abs . from . bfxUnRound

instance ToRequestParam (Rounded (QuotePerBase act)) where
  toTextParam =
    toTextParam
      . from @(QuotePerBase act) @(Ratio Natural)
      . bfxUnRound
