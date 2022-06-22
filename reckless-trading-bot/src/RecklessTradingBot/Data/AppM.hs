{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.AppM
  ( runApp,
  )
where

import qualified Data.Map as Map
import qualified RecklessTradingBot.Data.Env as EnvData
import RecklessTradingBot.Import

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
  getReportStartAmt = asks EnvData.envReportStartAmt
  getReportCurrency = asks EnvData.envReportCurrency
  getBaseBlacklist = asks EnvData.envBaseBlacklist
  getTeleEnv = asks EnvData.envTele
  getTradeVar = asks EnvData.envTrade
  getTradeEnv sym = do
    var <- asks EnvData.envTrade
    xs <- readMVar var
    case Map.lookup sym xs of
      Just x ->
        pure x
      Nothing ->
        throwE . ErrorRuntime $
          "Can not find TradeEnv for " <> show sym
  getExpiredOrders xs = do
    ct <- liftIO getCurrentTime
    ttl <- asks EnvData.envOrderTtl
    pure $
      filter
        ( \x ->
            ct
              > addUTCTime
                (from ttl)
                (orderInsertedAt $ entityVal x)
        )
        xs
  putCurrMma x = do
    ch <- asks EnvData.envMmaChan
    var <- asks EnvData.envLastMma
    liftIO . atomically $ do
      writeTChan ch x
      void $ tryTakeTMVar var
      putTMVar var x
  rcvNextMma = do
    ch0 <- asks EnvData.envMmaChan
    --
    -- 'dupTChan' and 'readTChan' should not
    -- be used under the same 'atomically'
    -- because it will cause deadlock
    --
    liftIO $
      (atomically . readTChan)
        =<< atomically (dupTChan ch0)
  getLastMma = do
    var <- asks EnvData.envLastMma
    liftIO . atomically $ tryReadTMVar var
