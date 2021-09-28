module RecklessTradingBot.Class.Env
  ( Env (..),
  )
where

import qualified BitfinexClient as Bfx
import RecklessTradingBot.Import.External

class MonadIO m => Env m where
  sleepPriceTtl :: m ()
  withBfx :: (Bfx.Env -> m a) -> m a
