{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.Order (apply) where

import qualified BitfinexClient as Bfx
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
apply :: Env m => m ()
apply = do
  $(logTM) InfoS "Spawned"
  xs <-
    mapM (spawnLink . loop) . toList
      =<< getPairs
  liftIO . void $ waitAnyCancel xs

loop :: Env m => TradingConf -> m ()
loop cfg = do
  price <- rcvNextPrice
  print price
  mapM_ resolveOngoing =<< Order.getOngoing sym
  xs <- Price.getSeq sym
  -- Order.create price sym
  --
  -- TODO : !!!
  --
  print xs
  loop cfg
  where
    sym = tradingConfPair cfg

resolveOngoing ::
  Env m =>
  Entity Order ->
  m ()
resolveOngoing x = do
  res <- runExceptT $ resolveOngoingT x
  whenLeft res $ \err ->
    $(logTM) ErrorS $ logStr (show err :: Text)

resolveOngoingT ::
  Env m =>
  Entity Order ->
  ExceptT Error m ()
resolveOngoingT x =
  case entityVal x of
    order@( Order
              { orderExtRef = Just ref,
                orderStatus = OrderActive
              }
            ) -> do
        $(logTM) InfoS . logStr $
          "Updating " <> (show order :: Text)
        bfx <-
          withBfxT Bfx.getOrder ($ from ref)
        lift . Order.updateStatus id0 $
          Bfx.orderStatus bfx
    order@( Order
              { orderStatus = OrderNew
              }
            ) -> do
        $(logTM) ErrorS . logStr $
          "Cancelling unexpected "
            <> (show order :: Text)
        cid <-
          except
            . first (ErrorTryFrom . SomeException)
            $ tryFrom id0
        withBfxT
          Bfx.cancelOrderByClientId
          (\f -> void . f cid $ orderAt order)
        lift $
          Order.updateStatus id0 OrderCancelled
    order ->
      $(logTM) ErrorS . logStr $
        "Ignoring unexpected "
          <> (show order :: Text)
  where
    id0 = entityKey x
