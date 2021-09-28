{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import RecklessTradingBot.Data.AppM (runApp)
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Thread.Main as MainThread

main :: IO ()
main = do
  !rc <- rawConfig
  handleScribe <-
    mkHandleScribeWithFormatter
      ( case rawConfigLogFormat rc of
          Bracket -> bracketFormat
          Json -> jsonFormat
      )
      ColorIfTerminal
      stdout
      (permitItem InfoS)
      (rawConfigLogVerbosity rc)
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "RecklessTradingBot" (Environment $ rawConfigLogEnv rc)
  bracket mkLogEnv closeScribes $ \le ->
    runKatipContextT le (mempty :: LogContexts) mempty $ do
      !env <- newEnv rc
      res <- lift $ runApp env $ MainThread.loop
      $(logTM) ErrorS
        $ logStr
        $ "Terminate program with result " <> (show res :: Text)
