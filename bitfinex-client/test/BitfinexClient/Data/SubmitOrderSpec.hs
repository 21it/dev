{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
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
spec = before newEnv $
  describe "ToJSON" $
    itRight "Request" . const $ do
      rate <- tryFromT @(Ratio Natural) $ 1 % 1234
      let req =
            SubmitOrder.Request
              testAmt
              [currencyPair|ADABTC|]
              rate
              SubmitOrder.optsPostOnly
      liftIO $
        A.encode req
          `shouldBe` "{\"amount\":\"2.00200201\",\"flags\":4096,\"symbol\":\"tADABTC\",\"type\":\"EXCHANGE LIMIT\",\"price\":\"0.00081037\"}"
