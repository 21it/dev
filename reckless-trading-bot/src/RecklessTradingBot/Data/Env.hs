{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( TradeEnv (..),
    Env (..),
    withEnv,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Math as Bfx
import Control.Monad.Logger
  ( runNoLoggingT,
    runStdoutLoggingT,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Env
  ( Error (UnreadError),
    Mod,
    Var,
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
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External

data RawTradeEnv = RawTradeEnv
  { rawTradeEnvCurrencyKind ::
      Bfx.CurrencyKind,
    rawTradeEnvMinProfitPerOrder ::
      Bfx.ProfitRate,
    rawTradeEnvMaxQuoteInvestment ::
      Bfx.Money 'Bfx.Quote 'Bfx.Buy,
    rawTradeEnvMode ::
      TradeMode
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance FromJSON RawTradeEnv where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 11
        }

data TradeEnv = TradeEnv
  { tradeEnvCurrencyPair :: Bfx.CurrencyPair,
    tradeEnvCurrencyKind :: Bfx.CurrencyKind,
    tradeEnvMinProfitPerOrder :: Bfx.ProfitRate,
    tradeEnvMaxQuoteInvestment :: Bfx.Money 'Bfx.Quote 'Bfx.Buy,
    tradeEnvBaseFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Base,
    tradeEnvQuoteFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Quote,
    tradeEnvMinBuyAmt :: Bfx.Money 'Bfx.Base 'Bfx.Buy,
    tradeEnvMinSellAmt :: Bfx.Money 'Bfx.Base 'Bfx.Sell,
    tradeEnvMode :: TradeMode
  }
  deriving stock (Eq)

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envPairs :: [MVar TradeEnv],
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

data RawEnv = RawEnv
  { -- app
    rawEnvBfx :: Bfx.Env,
    rawEnvPairs :: Map Bfx.CurrencyPair RawTradeEnv,
    rawEnvPriceTtl :: Seconds,
    rawEnvOrderTtl :: Seconds,
    -- storage
    rawEnvLibpqConnStr :: ConnectionString,
    -- logging
    rawEnvLogEnv :: Text,
    rawEnvLogFormat :: LogFormat,
    rawEnvLogSeverity :: Severity,
    rawEnvLogVerbosity :: Verbosity
  }

sysRawConfig :: (MonadUnliftIO m) => m RawEnv
sysRawConfig = liftIO $ do
  env <- Bfx.sysEnv
  parse (header "RecklessTradingBot config") $
    RawEnv env
      <$> var
        (tradePairs <=< nonempty)
        "RECKLESS_TRADING_BOT_PAIRS"
        op
      <*> var
        ( err . tryFrom @Pico
            <=< auto
            <=< nonempty
        )
        "RECKLESS_TRADING_BOT_PRICE_TTL"
        op
      <*> var
        ( err . tryFrom @Pico
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
        "RECKLESS_TRADING_BOT_LOG_SEVERITY"
        op
      <*> var
        (auto <=< nonempty)
        "RECKLESS_TRADING_BOT_LOG_VERBOSITY"
        op
  where
    op :: Mod Var a
    op =
      keep <> help ""
    tradePairs ::
      String ->
      Either Env.Error (Map Bfx.CurrencyPair RawTradeEnv)
    tradePairs =
      first
        UnreadError
        . A.eitherDecodeStrict
        . C8.pack
    err ::
      ( Show a
      ) =>
      Either a b ->
      Either Env.Error b
    err =
      first $ UnreadError . show

withEnv :: forall m. (MonadUnliftIO m) => (Env -> m ()) -> m ()
withEnv this = do
  rc <- sysRawConfig
  let sev = rawEnvLogSeverity rc
  let connStr = rawEnvLibpqConnStr rc
  handleScribe <-
    liftIO $
      mkHandleScribeWithFormatter
        ( case rawEnvLogFormat rc of
            Bracket -> bracketFormat
            Json -> jsonFormat
        )
        ColorIfTerminal
        stdout
        (permitItem sev)
        (rawEnvLogVerbosity rc)
  let mkLogEnv :: m LogEnv =
        liftIO $
          registerScribe
            "stdout"
            handleScribe
            defaultScribeSettings
            =<< initLogEnv
              "RecklessTradingBot"
              (Environment $ rawEnvLogEnv rc)
  let mkSqlPool :: m (Pool SqlBackend) =
        liftIO $
          if sev > DebugS
            then
              runNoLoggingT $
                createPostgresqlPool connStr 10
            else
              runStdoutLoggingT $
                createPostgresqlPool connStr 10
  bracket mkLogEnv rmLogEnv $ \le ->
    bracket mkSqlPool rmSqlPool $ \pool -> do
      let bfx = rawEnvBfx rc
      priceChan <-
        liftIO $
          atomically newBroadcastTChan
      --
      -- TODO : separate thread to update Bfx.symbolsDetails
      --
      cfg <-
        newTradeConfList bfx $
          rawEnvPairs rc
      this
        Env
          { -- app
            envBfx = bfx,
            envPairs = cfg,
            envPriceTtl = rawEnvPriceTtl rc,
            envOrderTtl = rawEnvOrderTtl rc,
            envPriceChan = priceChan,
            -- storage
            envSqlPool = pool,
            -- logging
            envKatipLE = le,
            envKatipCTX = mempty,
            envKatipNS = mempty
          }
  where
    rmLogEnv :: LogEnv -> m ()
    rmLogEnv = void . liftIO . closeScribes
    rmSqlPool :: Pool a -> m ()
    rmSqlPool = liftIO . destroyAllResources

newTradeConfList ::
  ( MonadIO m
  ) =>
  Bfx.Env ->
  Map Bfx.CurrencyPair RawTradeEnv ->
  m [MVar TradeEnv]
newTradeConfList bfx syms = do
  ex <-
    runExceptT $
      (,)
        <$> Bfx.symbolsDetails
        <*> Bfx.feeSummary bfx
  case ex of
    Left e -> error $ show e
    Right (symDetails, feeDetails) ->
      mapM (newTradeConf symDetails feeDetails) $
        Map.assocs syms

newTradeConf ::
  ( MonadIO m
  ) =>
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  FeeSummary.Response ->
  (Bfx.CurrencyPair, RawTradeEnv) ->
  m (MVar TradeEnv)
newTradeConf symDetails feeDetails (sym, raw) =
  case Map.lookup sym symDetails of
    Nothing -> error $ "Missing " <> show sym
    Just cfg -> do
      let amtNoFee = Bfx.currencyPairMinOrderAmt cfg
      let amtWithFee =
            case Bfx.tweakMoneyPip
              =<< Bfx.addFee amtNoFee fee of
              Left e -> error $ show e
              Right x -> x
      liftIO . newMVar $
        TradeEnv
          { tradeEnvCurrencyPair =
              sym,
            tradeEnvCurrencyKind =
              cck,
            tradeEnvMinProfitPerOrder =
              rawTradeEnvMinProfitPerOrder raw,
            tradeEnvMaxQuoteInvestment =
              rawTradeEnvMaxQuoteInvestment raw,
            tradeEnvBaseFee =
              fee,
            tradeEnvQuoteFee =
              coerce fee,
            tradeEnvMinBuyAmt =
              amtWithFee,
            tradeEnvMinSellAmt =
              coerce amtNoFee,
            tradeEnvMode =
              rawTradeEnvMode raw
          }
  where
    cck = rawTradeEnvCurrencyKind raw
    fee = FeeSummary.getFee @'Bfx.Maker cck feeDetails
