{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Env
  ( Env (..),
    newEnv,
  )
where

import BitfinexClient.Data.Web
import BitfinexClient.Import.External
import Env (header, help, keep, nonempty, parse, str, var)

data Env = Env
  { envApiKey :: ApiKey,
    envPrvKey :: PrvKey
  }

newEnv :: MonadIO m => m Env
newEnv =
  liftIO $
    parse (header "BitfinexClient config") $
      Env
        <$> var (str <=< nonempty) "BITFINEX_API_KEY" op
        <*> var (str <=< nonempty) "BITFINEX_PRV_KEY" op
  where
    op = keep <> help ""
