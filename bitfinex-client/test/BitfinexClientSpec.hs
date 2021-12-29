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
                Money $ quOf 250000 MoneyBaseAmt,
              currencyPairMinOrderAmt =
                Money $ quOf 2 MoneyBaseAmt
            }
  itRight "marketAveragePrice succeeds" . const $ do
    let sym = [currencyPair|ADABTC|]
    buyRate <-
      Bitfinex.marketAveragePrice
        (testAmt :: Money 'Base 'Buy)
        sym
    sellRate <-
      Bitfinex.marketAveragePrice
        (testAmt :: Money 'Base 'Sell)
        sym
    liftIO $ buyRate `shouldSatisfy` (> coerce sellRate)
  itLeft "marketAveragePrice fails" . const $ do
    let amt = testAmt :: Money 'Base 'Buy
    let sym = [currencyPair|BTCADA|]
    Bitfinex.marketAveragePrice amt sym
  itRight
    "feeSummary succeeds"
    Bitfinex.feeSummary
  itRight "submitOrderMaker and cancelOrderById succeeds" $ \env -> do
    let amt = testAmt :: Money 'Base 'Buy
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
    Bitfinex.submitCounterOrderMaker
      env
      (OrderId 0)
      [feeRateMakerBase| 0.001 |]
      [feeRateMakerQuote| 0.001 |]
      [profitRate| 0.001 |]
      SubmitOrder.optsPostOnly
  itRight
    "wallets succeeds"
    Bitfinex.wallets

--  describe "End2End" $ do
--    itRight "submitOrderMaker" $ \env -> do
--      let amt = from @(Ratio Natural) 2.002002 :: Money 'Base 'Buy
--      let sym = [currencyPair|ADABTC|]
--      let opts = SubmitOrder.optsPostOnly
--      rate <- Bitfinex.marketAveragePrice amt sym
--      Bitfinex.submitOrderMaker env amt sym rate opts
--    itRight "submitCounterOrderMaker" $ \env ->
--      Bitfinex.submitCounterOrderMaker
--        env
--        (OrderId 0)
--        [feeRateMakerBase| 0.001 |]
--        [feeRateMakerQuote| 0.001 |]
--        [profitRate| 0.001 |]
--        SubmitOrder.optsPostOnly
--    itRight "dumpIntoQuoteMaker" $ \env ->
--      Bitfinex.dumpIntoQuoteMaker
--        env
--        [currencyPair|ADABTC|]
--        SubmitOrder.optsPostOnly
