{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( TradingConf (..),
    Env (..),
    withEnv,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
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
import RecklessTradingBot.Data.Model
import RecklessTradingBot.Data.Money
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

data TradingConf = TradingConf
  { tradingConfPair :: Bfx.CurrencyPair,
    tradingConfFee :: Bfx.FeeRate 'Bfx.Maker,
    tradingConfMinOrderAmt ::
      MVar (MoneyAmount 'Bfx.Base)
  }
  deriving stock (Eq)

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envPairs :: [TradingConf],
    envProfit :: Bfx.ProfitRate,
    envPriceTtl :: Seconds,
    envOrderTtl :: Seconds,
    envPriceChan :: TChan (Entity Price),
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
        ( err . Bfx.newProfitRate
            <=< auto
            <=< nonempty
        )
        "RECKLESS_TRADING_BOT_PROFIT"
        op
      <*> var
        ( err . newSeconds'
            <=< auto
            <=< nonempty
        )
        "RECKLESS_TRADING_BOT_PRICE_TTL"
        op
      <*> var
        ( err . newSeconds'
            <=< auto
            <=< nonempty
        )
        "RECKLESS_TRADING_BOT_ORDER_TTL"
        op
      <*> var
        (str <=< nonempty)
        "RECKLESS_TRADING_BOT_LIBPQ_CONN_STR"
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
    json =
      first
        UnreadError
        . A.eitherDecodeStrict
        . C8.pack
    err ::
      Show a =>
      Either a b ->
      Either Env.Error b
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
          $ createPostgresqlPool
            (rawConfigLibpqConnStr rc)
            10
  bracket mkLogEnv rmLogEnv $ \le ->
    bracket mkSqlPool rmSqlPool $ \pool -> do
      let bfx = rawConfigBfx rc
      priceChan <-
        liftIO $
          atomically newBroadcastTChan
      --
      -- TODO : separate thread to update Bfx.symbolsDetails
      --
      cfg <-
        newTradingConfSet bfx $
          rawConfigPairs rc
      this
        Env
          { -- app
            envBfx = bfx,
            envPairs = cfg,
            envProfit = rawConfigProfit rc,
            envPriceTtl = rawConfigPriceTtl rc,
            envOrderTtl = rawConfigOrderTtl rc,
            envPriceChan = priceChan,
            -- storage
            envSqlPool = pool,
            -- logging
            envKatipLE = le,
            envKatipCTX = mempty,
            envKatipNS = mempty
          }
  where
    rmLogEnv = void . liftIO . closeScribes
    rmSqlPool = liftIO . destroyAllResources

newTradingConfSet ::
  MonadIO m =>
  Bfx.Env ->
  Set Bfx.CurrencyPair ->
  m [TradingConf]
newTradingConfSet bfx syms = do
  ex <-
    runExceptT $
      (,)
        <$> Bfx.symbolsDetails
        <*> Bfx.feeSummary bfx
  case ex of
    Left e -> error $ show e
    Right (symDetails, feeDetails) ->
      mapM (newTradingConf symDetails feeDetails) $
        toList syms

newTradingConf ::
  MonadIO m =>
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  FeeSummary.Response ->
  Bfx.CurrencyPair ->
  m TradingConf
newTradingConf symDetails feeDetails sym =
  case Map.lookup sym symDetails of
    Nothing -> error $ "Missing " <> show sym
    Just cfg -> do
      let amtDef = Bfx.currencyPairMinOrderAmt cfg
      let amtInclFee = into @(Bfx.PosRat) amtDef / (1 - from fee)
      when (amtInclFee <= 0)
        . error
        $ "Wrong min Order " <> show amtInclFee
      amtVar <-
        liftIO . newMVar $ from amtInclFee
      pure $
        TradingConf
          { tradingConfPair = sym,
            tradingConfFee = fee,
            tradingConfMinOrderAmt = amtVar
          }
  where
    fee = FeeSummary.makerCrypto2CryptoFee feeDetails
