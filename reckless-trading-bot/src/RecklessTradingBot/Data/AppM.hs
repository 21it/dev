{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.AppM
  ( runApp,
  )
where

import qualified BitfinexClient as Bfx
import qualified RecklessTradingBot.Data.Env as EnvData
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Model.Price as Price

newtype AppM m a = AppM
  { unAppM :: ReaderT EnvData.Env m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader EnvData.Env
    )

runApp :: EnvData.Env -> AppM m a -> m a
runApp env app = runReaderT (unAppM app) env

instance (MonadUnliftIO m) => Katip (AppM m) where
  getLogEnv = asks EnvData.envKatipLE
  localLogEnv f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipLE = f (EnvData.envKatipLE s)}) m)

instance (MonadUnliftIO m) => KatipContext (AppM m) where
  getKatipContext = asks EnvData.envKatipCTX
  localKatipContext f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipCTX = f (EnvData.envKatipCTX s)}) m)
  getKatipNamespace = asks EnvData.envKatipNS
  localKatipNamespace f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipNS = f (EnvData.envKatipNS s)}) m)

instance (MonadUnliftIO m) => Storage (AppM m) where
  getSqlPool = asks EnvData.envSqlPool
  runSql query = do
    pool <- asks EnvData.envSqlPool
    runSqlPool query pool

instance (MonadUnliftIO m) => Env (AppM m) where
  withBfxT method args = do
    bfx <- asks EnvData.envBfx
    withExceptT ErrorBfx . args $ method bfx
  getPairs = asks EnvData.envPairs
  getProfit = asks EnvData.envProfit
  orderExpired x = do
    ttl <- asks EnvData.envOrderTtl
    ct <- liftIO getCurrentTime
    pure $
      ct > addUTCTime (unSeconds ttl) (orderAt x)
  putCurrPrice x = do
    ch <- asks EnvData.envPriceChan
    liftIO . atomically $ writeTChan ch x
  rcvNextPrice sym = do
    ch0 <- asks EnvData.envPriceChan
    liftIO $ do
      --
      -- 'dupTChan' and 'readTChan' should not
      -- be used under the same 'atomically'
      -- because it will cause deadlock
      --
      ch1 <- atomically $ dupTChan ch0
      waitForPrice ch1
    where
      waitForPrice chan = do
        ent <- atomically $ readTChan chan
        let price = entityVal ent
        if ( priceBase price
               == from (Bfx.currencyPairBase sym)
           )
          && ( priceQuote price
                 == from (Bfx.currencyPairQuote sym)
             )
          then pure ent
          else waitForPrice chan
  sleepPriceTtl sym = do
    wantedTtl <- asks EnvData.envPriceTtl
    mPrice <- Price.getLatest sym
    case mPrice of
      Nothing -> pure ()
      Just price -> do
        ct <- liftIO getCurrentTime
        case tryFrom
          . diffUTCTime ct
          . priceUpdatedAt
          $ entityVal price of
          Left e -> do
            $(logTM) ErrorS $ logStr (show e :: Text)
            sleep wantedTtl
          Right realTtl
            | realTtl < wantedTtl ->
              sleep $ wantedTtl - realTtl
          Right {} ->
            pure ()
