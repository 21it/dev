{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order
  ( apply,
    cancelUnexpected,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.CancelOrderMulti as BfxCancel
import qualified BitfinexClient.Data.SubmitOrder as Bfx
import qualified Data.Set as Set
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Order as Order
import qualified RecklessTradingBot.Model.Price as Price

-- | This thread **must** be the only one
-- which is doing insert/update access to Order
-- model. Bot business logic is quite simple,
-- and I assume there is the only one instance
-- running. This way I can avoid lock-by-row
-- transactional complexity of postgres
-- procedures, and do simple insert/update instead.
apply :: (Env m) => m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <- mapM (spawnLink . loop) =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: (Env m) => MVar TradeConf -> m ()
loop varCfg = do
  cfg <- liftIO $ readMVar varCfg
  let sym = tradeConfCurrencyPair cfg
  priceEnt <- rcvNextPrice sym
  withOperativeBfx $ do
    cancelUnexpected
      =<< Order.getByStatusLimit sym [OrderNew]
    when
      ( (tradeConfMode cfg)
          `elem` ([Speculate, BuyOnly] :: [TradeMode])
      )
      $ do
        totalInvestment <- Order.getTotalInvestment sym
        if totalInvestment < tradeConfMaxQuoteInvestment cfg
          then do
            let lim = 5 :: Int
            priceSeq <- Price.getLatestLimit (from lim) sym
            when
              ( length priceSeq == lim
                  && goodPriceSeq True priceSeq
              )
              $ placeOrder cfg priceEnt
          else
            $(logTM) InfoS $
              "Total investment "
                <> show totalInvestment
                <> " exceeded limit for "
                <> show sym
                <> ", ignoring new price."
  loop varCfg

cancelUnexpected :: (Env m) => [Entity Order] -> m ()
cancelUnexpected [] = pure ()
cancelUnexpected entities = do
  res <-
    runExceptT $
      cancelUnexpectedT entities
  whenLeft res $
    $(logTM) ErrorS . show

cancelUnexpectedT ::
  ( Env m
  ) =>
  [Entity Order] ->
  ExceptT Error m ()
cancelUnexpectedT [] = pure ()
cancelUnexpectedT entities = do
  $(logTM) ErrorS $ show entities
  cids <-
    mapM
      ( \(Entity id0 x) -> do
          id1 <- tryFromT id0
          pure (id1, orderInsertedAt x)
      )
      entities
  gids <-
    mapM tryFromT ids
  void $
    withBfxT
      Bfx.cancelOrderMulti
      ($ BfxCancel.ByOrderClientId $ Set.fromList cids)
  void $
    withBfxT
      Bfx.cancelOrderMulti
      ($ BfxCancel.ByOrderGroupId $ Set.fromList gids)
  lift $
    Order.updateStatus OrderUnexpected ids
  where
    ids =
      entityKey <$> entities

placeOrder ::
  ( Env m
  ) =>
  TradeConf ->
  Entity Price ->
  m ()
placeOrder cfg priceEnt = do
  res <-
    runExceptT $ do
      quoteBalance <-
        withBfxT
          Bfx.spendableExchangeBalance
          ($ Bfx.currencyPairQuote $ tradeConfCurrencyPair cfg)
      when (quoteBalance > enterLoss) $
        placeOrderT cfg priceEnt
  whenLeft res $
    $(logTM) ErrorS . show
  where
    enterPrice =
      priceBuy $ entityVal priceEnt
    enterGain =
      tradeConfMinBuyAmt cfg
    enterLoss =
      case Bfx.roundMoney' $
        Bfx.unQuotePerBase enterPrice
          |*| Bfx.unMoney enterGain of
        Left e -> error $ show e
        Right x -> x

placeOrderT ::
  ( Env m
  ) =>
  TradeConf ->
  Entity Price ->
  ExceptT Error m ()
placeOrderT cfg priceEnt = do
  orderEnt@(Entity orderId order) <-
    lift $
      Order.create cfg priceEnt
  $(logTM) InfoS . logStr $
    "Placing a new order " <> (show orderEnt :: Text)
  cid <- tryFromT orderId
  gid <- tryFromT orderId
  bfxOrder <-
    withBfxT
      Bfx.submitOrderMaker
      ( \cont -> do
          cont
            (orderGain order)
            (tradeConfCurrencyPair cfg)
            (priceBuy $ entityVal priceEnt)
            Bfx.optsPostOnly
              { Bfx.clientId = Just cid,
                Bfx.groupId = Just gid
              }
      )
  lift $
    Order.updateBfx orderId bfxOrder

-- | The price sequence is good if it decreased monotonously.
goodPriceSeq :: Bool -> [Entity Price] -> Bool
goodPriceSeq False _ =
  False
goodPriceSeq True (x0 : x1 : xs) =
  let p0 = priceBuy $ entityVal x0
      p1 = priceBuy $ entityVal x1
   in goodPriceSeq (p0 > p1) $ x1 : xs
goodPriceSeq isGood _ =
  isGood
