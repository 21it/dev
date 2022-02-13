{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.ChartSpec
  ( spec,
  )
where

import qualified RecklessTradingBot.Chart as Chart
import RecklessTradingBot.Import
import Test.Hspec

spec :: Spec
spec = do
  it "chart" $ do
    Chart.newExample
    True `shouldBe` True
