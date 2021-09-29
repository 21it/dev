{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBotSpec
  ( spec,
  )
where

import RecklessTradingBot.Import
import Test.Hspec

spec :: Spec
spec = do
  it "success" $ do
    True `shouldBe` True
