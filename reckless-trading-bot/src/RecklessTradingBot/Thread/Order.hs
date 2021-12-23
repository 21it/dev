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
  cancelUnexpected . (fst <$>)
    =<< Order.getByStatus sym [OrderNew]
  priceSeq <- Price.getSeq sym
  when (goodPriceSeq priceSeq) $ do
    orderId <- entityKey <$> Order.create priceEnt
    placeOrder cfg orderId price
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
  lift $ Order.updateStatus OrderUnexpected ids
  where
    ids = entityKey <$> entities

placeOrder ::
  ( Env m
  ) =>
  TradeConf ->
  OrderId ->
  Price ->
  m ()
placeOrder cfg orderId price = do
  res <-
    runExceptT $
      placeOrderT cfg orderId price
  whenLeft res $
    $(logTM) ErrorS . show

placeOrderT ::
  ( Env m
  ) =>
  TradeConf ->
  OrderId ->
  Price ->
  ExceptT Error m ()
placeOrderT cfg orderId price = do
  cid <- tryFromT orderId
  gid <- tryFromT orderId
  bfxOrder <-
    withBfxT
      Bfx.submitOrderMaker
      ( \cont -> do
          cont
            (tradeConfMinBuyAmt cfg)
            (tradeConfCurrencyPair cfg)
            (priceBuy price)
            Bfx.optsPostOnly
              { Bfx.clientId = Just cid,
                Bfx.groupId = Just gid
              }
      )
  lift $
    Order.bfxUpdate orderId bfxOrder

--
-- TODO : !!!
--
goodPriceSeq :: [Entity Price] -> Bool
goodPriceSeq xs | length xs >= 3 = True
goodPriceSeq _ = False
