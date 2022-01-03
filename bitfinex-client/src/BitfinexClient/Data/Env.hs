{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Env
  ( Env (..),
    newEnv,
  )
where

import BitfinexClient.Data.Web
import BitfinexClient.Import.External
import Env
  ( Mod,
    Var,
    header,
    help,
    keep,
    nonempty,
    parse,
    str,
    var,
  )

data Env = Env
  { envNonceGen :: NonceGen,
    envApiKey :: ApiKey,
    envPrvKey :: PrvKey
  }
  deriving stock
    ( Eq,
      -- | It's safe to derive 'Show' instance,
      -- because 'NonceGen', 'ApiKey' and 'PrvKey'
      -- instances are safe.
      Show
    )

newEnv ::
  ( MonadIO m
  ) =>
  m Env
newEnv = do
  nonceGen <- newNonceGen
  liftIO
    . parse (header "BitfinexClient config")
    $ Env nonceGen
      <$> var (str <=< nonempty) "BITFINEX_API_KEY" op
      <*> var (str <=< nonempty) "BITFINEX_PRV_KEY" op
  where
    op :: Mod Var a
    op = keep <> help ""
