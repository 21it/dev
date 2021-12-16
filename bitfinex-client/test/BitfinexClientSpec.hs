{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClientSpec
  ( spec,
  )
where

import qualified BitfinexClient as Bitfinex
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import BitfinexClient.TestEnv
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = before newEnv $ do
  itRight "symbolsDetails succeeds" . const $ do
    ss <- Bitfinex.symbolsDetails
    liftIO $
      Map.lookup [currencyPair|ADABTC|] ss
        `shouldBe` Just
          CurrencyPairConf
            { currencyPairPrecision = 5,
              currencyPairInitMargin = 30 % 1,
              currencyPairMinMargin = 15,
              currencyPairMaxOrderAmt =
                MoneyAmt $ quOf 250000 MoneyBaseAmt,
              currencyPairMinOrderAmt =
                MoneyAmt $ quOf 2 MoneyBaseAmt
            }
  itRight "marketAveragePrice succeeds" . const $ do
    let amt = testAmt
    let sym = [currencyPair|ADABTC|]
    buy <- Bitfinex.marketAveragePrice amt sym
    sell <- Bitfinex.marketAveragePrice amt sym
    liftIO $ buy `shouldSatisfy` (> sell)
  itLeft "marketAveragePrice fails" . const $ do
    let amt = MoneyAmt $ quOf 2 MoneyBaseAmt :: MoneyBase 'Buy
    let sym = [currencyPair|BTCADA|]
    Bitfinex.marketAveragePrice amt sym
  itRight
    "feeSummary succeeds"
    Bitfinex.feeSummary
  itRight "submitOrderMaker and cancelOrderById succeeds" $ \env -> do
    let amt = testAmt
    let sym = [currencyPair|ADABTC|]
    let opts = SubmitOrder.optsPostOnly
    rate <- Bitfinex.marketAveragePrice amt sym
    order <- Bitfinex.submitOrderMaker env amt sym rate opts
    Bitfinex.cancelOrderById env $ orderId order
  itRight "retrieveOrders succeeds" $ \env ->
    Bitfinex.retrieveOrders env $
      GetOrders.optsSym [currencyPair|ADABTC|]
  itRight "ordersHistory succeeds" $ \env ->
    Bitfinex.ordersHistory env $
      GetOrders.optsSym [currencyPair|ADABTC|]
  itRight "getOrders succeeds" $ \env ->
    Bitfinex.getOrders env $
      GetOrders.optsSym [currencyPair|ADABTC|]
  itLeft "getOrder fails" $ \env ->
    Bitfinex.getOrder env $ OrderId 0
  itLeft "submitCounterOrderMaker fails" $ \env -> do
    prof <- tryFromT @Rational $ 1 % 1000
    Bitfinex.submitCounterOrderMaker
      env
      (OrderId 0)
      [feeRateMakerBase| 0.001 |]
      prof
      SubmitOrder.optsPostOnly
  itRight
    "wallets succeeds"
    Bitfinex.wallets

--itRight "dumpIntoQuoteMaker succeeds" $ \env ->
--  Bitfinex.dumpIntoQuoteMaker
--    env
--    [currencyPair|ADABTC|]
--    SubmitOrder.optsPostOnly
