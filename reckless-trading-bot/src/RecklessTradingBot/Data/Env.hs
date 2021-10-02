{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( Env (..),
    withEnv,
  )
where

import qualified BitfinexClient as Bfx
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import Env
  ( Error (UnreadError),
    auto,
    header,
    help,
    keep,
    nonempty,
    parse,
    str,
    var,
  )
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envPairs :: Set Bfx.CurrencyPair,
    envProfit :: Bfx.ProfitRate,
    envPriceTtl :: Seconds,
    envOrderTtl :: Seconds,
    -- storage
    envSqlPool :: Pool SqlBackend,
    -- logging
    envKatipNS :: Namespace,
    envKatipCTX :: LogContexts,
    envKatipLE :: LogEnv
  }

data RawConfig = RawConfig
  { -- app
    rawConfigBfx :: Bfx.Env,
    rawConfigPairs :: Set Bfx.CurrencyPair,
    rawConfigProfit :: Bfx.ProfitRate,
    rawConfigPriceTtl :: Seconds,
    rawConfigOrderTtl :: Seconds,
    -- storage
    rawConfigLibpqConnStr :: ConnectionString,
    -- logging
    rawConfigLogEnv :: Text,
    rawConfigLogFormat :: LogFormat,
    rawConfigLogVerbosity :: Verbosity
  }

newRawConfig :: MonadUnliftIO m => m RawConfig
newRawConfig = liftIO $ do
  env <- Bfx.newEnv
  parse (header "RecklessTradingBot config") $
    RawConfig env
      <$> var
        (json <=< nonempty)
        "RECKLESS_TRADING_BOT_PAIRS"
        op
      <*> var
        (err . Bfx.newProfitRate <=< auto <=< nonempty)
        "RECKLESS_TRADING_BOT_PROFIT"
        op
      <*> var
        (err . newSeconds <=< auto <=< nonempty)
        "RECKLESS_TRADING_BOT_PRICE_TTL"
        op
      <*> var
        (err . newSeconds <=< auto <=< nonempty)
        "RECKLESS_TRADING_BOT_ORDER_TTL"
        op
      <*> var
        (str <=< nonempty)
        "RECKLESS_TRADING_LIBPQ_CONN_STR"
        op
      <*> var
        (str <=< nonempty)
        "RECKLESS_TRADING_BOT_LOG_ENV"
        op
      <*> var
        (auto <=< nonempty)
        "RECKLESS_TRADING_BOT_LOG_FORMAT"
        op
      <*> var
        (auto <=< nonempty)
        "RECKLESS_TRADING_BOT_LOG_VERBOSITY"
        op
  where
    op = keep <> help ""
    json = first UnreadError . A.eitherDecodeStrict . C8.pack
    err :: Show a => Either a b -> Either Env.Error b
    err = first $ UnreadError . show

withEnv :: MonadUnliftIO m => (Env -> m ()) -> m ()
withEnv this = do
  rc <- newRawConfig
  handleScribe <-
    liftIO $
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
        liftIO $
          registerScribe
            "stdout"
            handleScribe
            defaultScribeSettings
            =<< initLogEnv
              "RecklessTradingBot"
              (Environment $ rawConfigLogEnv rc)
  let mkSqlPool =
        liftIO
          . runNoLoggingT
          $ createPostgresqlPool (rawConfigLibpqConnStr rc) 10
  bracket mkLogEnv (void . liftIO . closeScribes) $ \le ->
    bracket mkSqlPool (liftIO . destroyAllResources) $ \pool ->
      this
        Env
          { -- app
            envBfx = rawConfigBfx rc,
            envPairs = rawConfigPairs rc,
            envProfit = rawConfigProfit rc,
            envPriceTtl = rawConfigPriceTtl rc,
            envOrderTtl = rawConfigOrderTtl rc,
            -- storage
            envSqlPool = pool,
            -- logging
            envKatipLE = le,
            envKatipCTX = mempty,
            envKatipNS = mempty
          }
