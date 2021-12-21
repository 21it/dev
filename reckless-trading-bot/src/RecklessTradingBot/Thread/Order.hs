{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order
  ( apply,
    cancelUnexpected,
    cancelUnexpectedT,
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
  priceEnt@(Entity _ price) <- rcvNextPrice sym
  cancelUnexpected =<< Order.getByStatus sym [OrderNew]
  priceSeq <- Price.getSeq sym
  when (goodPriceSeq priceSeq) $ do
    orderId <- entityKey <$> Order.create priceEnt
    placeOrder
      orderId
      (tradeConfMinBuyAmt cfg)
      sym
      price
      (tradeConfBaseFee cfg)
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
          pure (id1, orderAt x)
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
  lift $ Order.updateStatus OrderUnexpected ids
  where
    ids = entityKey <$> entities

placeOrder ::
  ( Env m
  ) =>
  OrderId ->
  Bfx.MoneyBase 'Bfx.Buy ->
  Bfx.CurrencyPair ->
  Price ->
  Bfx.FeeRate 'Bfx.Maker 'Bfx.Base ->
  m ()
placeOrder rowId amt sym price fee = do
  res <-
    runExceptT $ do
      (bfxOrder, ss) <- placeOrderT rowId amt sym price fee
      when (ss /= OrderActive) $
        $(logTM) ErrorS . logStr $
          "Unexpected status "
            <> (show ss :: Text)
            <> " of Bitfinex order "
            <> show bfxOrder
  whenLeft res $ \e ->
    $(logTM) ErrorS . logStr $
      "Order failed " <> (show e :: Text)

placeOrderT ::
  ( Env m
  ) =>
  OrderId ->
  Bfx.MoneyBase 'Bfx.Buy ->
  Bfx.CurrencyPair ->
  Price ->
  Bfx.FeeRate 'Bfx.Maker 'Bfx.Base ->
  ExceptT
    Error
    m
    ( Bfx.Order 'Bfx.Buy 'Bfx.Remote,
      OrderStatus
    )
placeOrderT rowId amt sym price _ = do
  cid <- tryFromT rowId
  bfxOrder <-
    withBfxT
      Bfx.submitOrderMaker
      ( \f -> do
          f
            (from amt)
            sym
            (from $ priceBuy price :: Bfx.QuotePerBase 'Bfx.Buy)
            Bfx.optsPostOnly
              { Bfx.clientId = Just cid
              }
      )
  ss <-
    lift
      . Order.updateBfx rowId
      $ Bfx.SomeOrder sing bfxOrder
  pure (bfxOrder, ss)

--
-- TODO : !!!
--
goodPriceSeq :: [Entity Price] -> Bool
goodPriceSeq xs | length xs >= 3 = True
goodPriceSeq _ = False
