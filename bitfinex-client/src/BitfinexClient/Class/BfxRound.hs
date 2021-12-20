{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.BfxRound
  ( BfxRound (..),
  )
where

import BitfinexClient.Data.Metro
import BitfinexClient.Import.External

class (From a Rational, TryFrom Rational a) => BfxRound a where
  bfxRound :: a -> Either (TryFromException Rational a) a

instance BfxRound (QuotePerBase act) where
  bfxRound =
    tryFrom
      . sdRound 5
      . dpRound 8
      . from

instance BfxRound (MoneyAmt dim act) where
  bfxRound =
    tryFrom
      . dpRound 8
      . from
