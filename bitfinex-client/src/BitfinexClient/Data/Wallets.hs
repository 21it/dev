{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Wallets
  ( WalletType,
    newWalletType,
    Response (..),
  )
where

import BitfinexClient.Data.Metro
import BitfinexClient.Import.External

data WalletType
  = Exchange
  | Margin
  | Funding
  deriving stock (Eq, Ord, Show)

newWalletType ::
  Text ->
  Either (TryFromException Text WalletType) WalletType
newWalletType = \case
  "exchange" -> Right Exchange
  "margin" -> Right Margin
  "funding" -> Right Funding
  x -> Left $ TryFromException x Nothing

data Response = Response
  { balance :: MoneyBase,
    unsettledInterest :: MoneyBase,
    availableBalance :: MoneyBase,
    lastChange :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)
