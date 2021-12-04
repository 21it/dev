{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.TypeSpec
  ( spec,
  )
where

import BitfinexClient.Import
import Test.Hspec

spec :: Spec
spec = do
  describe "Orders" $ do
    it "unOrderFlag succeeds" $
      unOrderFlag PostOnly
        `shouldBe` OrderFlagAcc 4096
    it "unOrderFlagSet succeeds" $
      unOrderFlagSet [Hidden, PostOnly]
        `shouldBe` OrderFlagAcc 4160
  describe "Trading" $ do
    it "newCurrencyPair succeeds" $
      newCurrencyPair "ADA" "BTC" `shouldSatisfy` isRight
    it "newCurrencyPair fails" $
      newCurrencyPair "BTC" "BTC" `shouldSatisfy` isLeft
  describe "Misc" $ do
    it "PosRat succeeds" $
      tryFrom @Rational @PosRat 1 `shouldSatisfy` isRight
    it "PosRat fails" $
      tryFrom @Rational @PosRat 0 `shouldSatisfy` isLeft
