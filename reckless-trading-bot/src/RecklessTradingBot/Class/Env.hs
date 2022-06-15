{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Class.Env
  ( Env (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Class.Storage
import qualified RecklessTradingBot.Data.Env as EnvData
import RecklessTradingBot.Data.Model
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

class (Storage m, KatipContext m) => Env m where
  withBfx ::
    (Bfx.Env -> a) ->
    (a -> ExceptT Bfx.Error m b) ->
    m (Either Error b)
  withBfx method = runExceptT . withBfxT method
  withBfxT ::
    (Bfx.Env -> a) ->
    (a -> ExceptT Bfx.Error m b) ->
    ExceptT Error m b
  getTeleEnv :: m EnvData.TeleEnv
  getTradeCfg :: ExceptT Error m EnvData.TradeEnv
  getExpiredOrders :: [Entity Order] -> m [Entity Order]
  putCurrMma :: Bfx.Mma -> m ()
  rcvNextMma :: m Bfx.Mma
  getLastMma :: m (Maybe Bfx.Mma)
  getReportStartAmt :: m (Bfx.Money 'Bfx.Quote 'Bfx.Sell)
  getReportCurrency :: m (Bfx.CurrencyCode 'Bfx.Quote)
