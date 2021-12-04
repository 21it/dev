{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.FeeSummary
  ( Response (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

data Response = Response
  { makerCrypto2CryptoFee :: FeeRate 'Maker 'Base,
    makerCrypto2StableFee :: FeeRate 'Maker 'Base,
    makerCrypto2FiatFee :: FeeRate 'Maker 'Base,
    makerDerivativeRebate :: RebateRate 'Maker,
    takerCrypto2CryptoFee :: FeeRate 'Taker 'Base,
    takerCrypto2StableFee :: FeeRate 'Taker 'Base,
    takerCrypto2FiatFee :: FeeRate 'Taker 'Base,
    takerDerivativeFee :: FeeRate 'Taker 'Base
  }
  deriving stock (Eq, Ord, Show)
