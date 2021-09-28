{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.FeeSummary
  ( Response (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

data Response = Response
  { makerCrypto2CryptoFee :: FeeRate 'Maker,
    makerCrypto2StableFee :: FeeRate 'Maker,
    makerCrypto2FiatFee :: FeeRate 'Maker,
    makerDerivativeRebate :: RebateRate 'Maker,
    takerCrypto2CryptoFee :: FeeRate 'Taker,
    takerCrypto2StableFee :: FeeRate 'Taker,
    takerCrypto2FiatFee :: FeeRate 'Taker,
    takerDerivativeFee :: FeeRate 'Taker
  }
  deriving stock (Eq, Ord, Show)
