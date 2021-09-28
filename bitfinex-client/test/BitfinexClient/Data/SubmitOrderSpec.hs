{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.SubmitOrderSpec
  ( spec,
  )
where

import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import BitfinexClient.TestEnv
import qualified Data.Aeson as A
import Test.Hspec

spec :: Spec
spec =
  describe "ToJSON" $
    it "Request" $ do
      x <- withAdaBtc $ \amt sym -> do
        rate <- except . newExchangeRate $ 1 % 1234
        let opts = SubmitOrder.optsPostOnly
            req = SubmitOrder.Request Buy amt sym rate opts
        lift $
          A.encode req
            `shouldBe` "{\"amount\":\"2.00200201\",\"flags\":4096,\"symbol\":\"tADABTC\",\"type\":\"EXCHANGE LIMIT\",\"price\":\"0.00081037\"}"
      x `shouldSatisfy` isRight
