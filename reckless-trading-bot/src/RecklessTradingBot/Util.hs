{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Util
  ( spawnLink,
    withOperativeBfx,
  )
where

import qualified BitfinexClient as Bfx
import Control.Concurrent.Async
  ( Async (..),
    async,
    link,
  )
import RecklessTradingBot.Data.Time (seconds, sleep)
import RecklessTradingBot.Import.External

spawnLink :: (MonadUnliftIO m) => m a -> m (Async a)
spawnLink x =
  withRunInIO $ \run -> do
    pid <- async $ run x
    link pid
    pure pid

withOperativeBfx :: (KatipContext m) => m () -> m ()
withOperativeBfx action = do
  res <- runExceptT Bfx.platformStatus
  case res of
    Right Bfx.PltOperative ->
      action
    Left e@(Bfx.ErrorWebException {}) -> do
      $(logTM) WarningS $ logStr (show e :: Text)
      sleep [seconds|30|]
    e -> do
      $(logTM) ErrorS $ logStr (show e :: Text)
      sleep [seconds|30|]
