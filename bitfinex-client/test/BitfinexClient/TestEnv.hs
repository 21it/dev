{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.TestEnv
  ( eraseFirst,
    testAmt,
    itRight,
    itLeft,
  )
where

import BitfinexClient.Import
import qualified Test.Hspec as HS

eraseFirst :: Bifunctor f => f a b -> f () b
eraseFirst =
  first $ const ()

testAmt :: MoneyBase 'Buy
testAmt =
  from @(Ratio Natural) $
    200200201 % 100000000

itRight ::
  ( Show a
  ) =>
  String ->
  (Env -> ExceptT Error IO a) ->
  HS.SpecWith (HS.Arg (Env -> IO ()))
itRight label test =
  HS.it label $ \env -> do
    x <- runExceptT $ test env
    x `HS.shouldSatisfy` isRight

itLeft ::
  ( Show a
  ) =>
  String ->
  (Env -> ExceptT Error IO a) ->
  HS.SpecWith (HS.Arg (Env -> IO ()))
itLeft label test =
  HS.it label $ \env -> do
    x <- runExceptT $ test env
    x `HS.shouldSatisfy` isLeft
