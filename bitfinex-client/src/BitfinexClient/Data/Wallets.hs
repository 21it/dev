{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Wallets
  ( WalletType (..),
    newWalletType,
    Response (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Import.External

data WalletType
  = Exchange
  | Margin
  | Funding
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      Enum,
      Bounded
    )

newWalletType ::
  Text ->
  Either (TryFromException Text WalletType) WalletType
newWalletType = \case
  "exchange" -> Right Exchange
  "margin" -> Right Margin
  "funding" -> Right Funding
  x -> Left $ TryFromException x Nothing

data Response = Response
  { balance :: Money 'Base 'Sell,
    unsettledInterest :: Money 'Base 'Sell,
    availableBalance :: Money 'Base 'Sell,
    lastChange :: Maybe Text
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
