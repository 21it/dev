{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.AppM
  ( runApp,
  )
where

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

instance (MonadIO m) => Katip (AppM m) where
  getLogEnv = asks EnvData.envKatipLE
  localLogEnv f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipLE = f (EnvData.envKatipLE s)}) m)

instance (MonadIO m) => KatipContext (AppM m) where
  getKatipContext = asks EnvData.envKatipCTX
  localKatipContext f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipCTX = f (EnvData.envKatipCTX s)}) m)
  getKatipNamespace = asks EnvData.envKatipNS
  localKatipNamespace f (AppM m) =
    AppM (local (\s -> s {EnvData.envKatipNS = f (EnvData.envKatipNS s)}) m)

instance (MonadIO m) => Env (AppM m) where
  withBfx = (asks EnvData.envBfx >>=)
  getPairs = asks EnvData.envPairs
  getProfit = asks EnvData.envProfit
  getOrderTtl = asks EnvData.envOrderTtl
  sleepPriceTtl = asks EnvData.envPriceTtl >>= sleep
