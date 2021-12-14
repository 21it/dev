{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order (apply) where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.SubmitOrder as Bfx
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
apply ::
  ( Env m
  ) =>
  m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <-
    mapM (spawnLink . loop) . toList
      =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop ::
  ( Env m
  ) =>
  TradingConf ->
  m ()
loop cfg = do
  priceEnt@(Entity _ price) <- rcvNextPrice sym
  resolved <- mapM (resolveOngoing fee) =<< Order.getOngoing sym
  when (getAll $ mconcat resolved) $ do
    seq0 <- Price.getSeq sym
    when (goodPriceSeq seq0) $ do
      amt <- readMVar $ tradingConfMinOrderAmt cfg
      orderRowId <- entityKey <$> Order.create priceEnt
      placeOrder orderRowId amt sym price fee
  loop cfg
  where
    sym = tradingConfPair cfg
    fee = tradingConfFee cfg

resolveOngoing ::
  Env m =>
  Bfx.FeeRate 'Bfx.Maker 'Bfx.Base ->
  Entity Order ->
  m All
resolveOngoing fee order = do
  res <- runExceptT $ resolveOngoingT fee order
  case res of
    Left err -> do
      $(logTM) ErrorS . logStr $
        "Resolve ongoing failed " <> (show err :: Text)
      pure $ All False
    Right ss ->
      pure . All $ finalStatus ss

resolveOngoingT ::
  Env m =>
  Bfx.FeeRate 'Bfx.Maker 'Bfx.Base ->
  Entity Order ->
  ExceptT Error m OrderStatus
resolveOngoingT _ ent@(Entity rowId row) =
  case orderExtRef row of
    Just ref | ss == OrderActive -> do
      $(logTM) DebugS . logStr $
        "Updating " <> (show ent :: Text)
      bfxOrder <- withBfxT Bfx.getOrder ($ from ref)
      lift $ Order.updateBfx rowId bfxOrder
    _ | ss == OrderNew -> do
      $(logTM) ErrorS . logStr $
        "Cancelling unexpected " <> (show ent :: Text)
      cid <- tryFromT rowId
      res <-
        withBfxT
          Bfx.cancelOrderByClientId
          (\f -> f cid $ orderAt row)
      case res of
        Just bfxOrder ->
          lift $ Order.updateBfx rowId bfxOrder
        Nothing -> do
          $(logTM) ErrorS . logStr $
            "Nonexistent " <> (show ent :: Text)
          pure OrderCancelled
    _ -> do
      $(logTM) ErrorS . logStr $
        "Ignoring unexpected " <> (show ent :: Text)
      pure ss
  where
    ss :: OrderStatus
    ss = from $ orderStatus row

placeOrder ::
  ( Env m
  ) =>
  OrderId ->
  MoneyBase 'Bfx.Buy ->
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
  whenLeft res $ \err ->
    $(logTM) ErrorS . logStr $
      "Order failed " <> (show err :: Text)

placeOrderT ::
  ( Env m
  ) =>
  OrderId ->
  MoneyBase 'Bfx.Buy ->
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
