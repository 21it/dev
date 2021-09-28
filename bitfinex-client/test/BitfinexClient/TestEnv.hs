{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.TestEnv
  ( withAdaBtc,
  )
where

import BitfinexClient.Import

withAdaBtc ::
  Monad m =>
  (MoneyAmount -> CurrencyPair -> ExceptT Error m a) ->
  m (Either Error a)
withAdaBtc this = runExceptT $ do
  amt <- except . newMoneyAmount $ 200200201 % 100000000
  sym <- except $ newCurrencyPair "ADA" "BTC"
  this amt sym
