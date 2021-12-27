{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.BfxRoundSpec
  ( spec,
  )
where

import BitfinexClient.Import
import BitfinexClient.TestEnv
import Test.Hspec

spec :: Spec
spec = do
  describe "BfxRound" $ do
    it "MoneyAmt" $
      eraseFirst
        ( from @(Rounded (MoneyBase 'Buy))
            <$> bfxRound testAmt
        )
        `shouldBe` Right (testAmt :: MoneyBase 'Buy)
