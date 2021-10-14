{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Util
  ( spawnLink,
  )
where

import Control.Concurrent.Async
  ( Async (..),
    async,
    link,
  )
import RecklessTradingBot.Import.External

spawnLink :: MonadUnliftIO m => m a -> m (Async a)
spawnLink x =
  withRunInIO $ \run -> do
    pid <- async $ run x
    link pid
    return pid
