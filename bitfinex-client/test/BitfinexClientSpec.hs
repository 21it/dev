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
import Test.Hspec

spec :: Spec
spec = before newEnv $ do
  it "marketAveragePrice succeeds" . const $ do
    x <- withAdaBtc $ \amt sym -> do
      buy <- Bitfinex.marketAveragePrice Buy amt sym
      sell <- Bitfinex.marketAveragePrice Sell amt sym
      liftIO $ buy `shouldSatisfy` (> sell)
    x `shouldSatisfy` isRight
  it "marketAveragePrice fails" . const $ do
    x <- runExceptT $ do
      amt <- except $ newMoneyAmount 2
      sym <- except $ newCurrencyPair "BTC" "ADA"
      Bitfinex.marketAveragePrice Buy amt sym
    x `shouldSatisfy` isLeft
  it "feeSummary succeeds" $ \env -> do
    x <- runExceptT $ Bitfinex.feeSummary env
    x `shouldSatisfy` isRight
  it "submitOrderMaker and cancelOrderById succeeds" $ \env -> do
    x <- withAdaBtc $ \amt sym -> do
      rate <- Bitfinex.marketAveragePrice Buy amt sym
      let opts = SubmitOrder.optsPostOnly
      order <- Bitfinex.submitOrderMaker env Buy amt sym rate opts
      Bitfinex.cancelOrderById env $ orderId order
    x `shouldSatisfy` isRight
  it "retrieveOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.retrieveOrders env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "ordersHistory succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.ordersHistory env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "getOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.getOrders env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "getOrder fails" $ \env -> do
    x <- runExceptT $ Bitfinex.getOrder env $ OrderId 0
    x `shouldSatisfy` isLeft
  it "submitCounterOrderMaker fails" $ \env -> do
    x <-
      runExceptT $ do
        rate <- except . newPosRat $ 1 % 1000
        Bitfinex.submitCounterOrderMaker
          env
          (OrderId 0)
          (FeeRate rate)
          (ProfitRate rate)
          SubmitOrder.optsPostOnly
    x `shouldSatisfy` isLeft
