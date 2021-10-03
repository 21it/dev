{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.AppM
  ( runApp,
  )
where

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
  withBfx = (asks EnvData.envBfx >>=)
  getPairs = asks EnvData.envPairs
  getProfit = asks EnvData.envProfit
  getOrderTtl = asks EnvData.envOrderTtl
  putCurrPrice x = do
    ch <- asks EnvData.envPriceChan
    liftIO . atomically $ writeTChan ch x
  rcvNextPrice = do
    ch <- asks EnvData.envPriceChan
    liftIO . atomically $ dupTChan ch >>= readTChan
  sleepTillNextPrice sym = do
    ttl0 <- asks EnvData.envPriceTtl
    mx <- Price.getLatest sym
    case mx of
      Nothing -> pure ()
      Just x -> do
        ct <- liftIO getCurrentTime
        case newSeconds'
          . diffUTCTime ct
          . priceAt
          $ entityVal x of
          --
          -- TODO : add unexpected error logging
          --
          Left {} -> sleep ttl0
          Right ttl1 | ttl1 < ttl0 -> sleep $ ttl0 - ttl1
          Right {} -> pure ()
