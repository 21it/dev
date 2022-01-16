{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( TradeConf (..),
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

data RawTradeConf = RawTradeConf
  { rawTradeConfCurrencyKind ::
      Bfx.CurrencyKind,
    rawTradeConfMinProfitPerOrder ::
      Bfx.ProfitRate,
    rawTradeConfMaxQuoteInvestment ::
      Bfx.Money 'Bfx.Quote 'Bfx.Buy,
    rawTradeConfMode ::
      TradeMode
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance FromJSON RawTradeConf where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 12
        }

data TradeConf = TradeConf
  { tradeConfCurrencyPair :: Bfx.CurrencyPair,
    tradeConfCurrencyKind :: Bfx.CurrencyKind,
    tradeConfMinProfitPerOrder :: Bfx.ProfitRate,
    tradeConfMaxQuoteInvestment :: Bfx.Money 'Bfx.Quote 'Bfx.Buy,
    tradeConfBaseFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Base,
    tradeConfQuoteFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Quote,
    tradeConfMinBuyAmt :: Bfx.Money 'Bfx.Base 'Bfx.Buy,
    tradeConfMinSellAmt :: Bfx.Money 'Bfx.Base 'Bfx.Sell,
    tradeConfMode :: TradeMode
  }
  deriving stock (Eq)

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envPairs :: [MVar TradeConf],
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
    rawConfigPairs :: Map Bfx.CurrencyPair RawTradeConf,
    rawConfigPriceTtl :: Seconds,
    rawConfigOrderTtl :: Seconds,
    -- storage
    rawConfigLibpqConnStr :: ConnectionString,
    -- logging
    rawConfigLogEnv :: Text,
    rawConfigLogFormat :: LogFormat,
    rawConfigLogSeverity :: Severity,
    rawConfigLogVerbosity :: Verbosity
  }

newRawConfig :: (MonadUnliftIO m) => m RawConfig
newRawConfig = liftIO $ do
  env <- Bfx.newEnv
  parse (header "RecklessTradingBot config") $
    RawConfig env
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
      Either Env.Error (Map Bfx.CurrencyPair RawTradeConf)
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
  rc <- newRawConfig
  let sev = rawConfigLogSeverity rc
  let connStr = rawConfigLibpqConnStr rc
  handleScribe <-
    liftIO $
      mkHandleScribeWithFormatter
        ( case rawConfigLogFormat rc of
            Bracket -> bracketFormat
            Json -> jsonFormat
        )
        ColorIfTerminal
        stdout
        (permitItem sev)
        (rawConfigLogVerbosity rc)
  let mkLogEnv :: m LogEnv =
        liftIO $
          registerScribe
            "stdout"
            handleScribe
            defaultScribeSettings
            =<< initLogEnv
              "RecklessTradingBot"
              (Environment $ rawConfigLogEnv rc)
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
      let bfx = rawConfigBfx rc
      priceChan <-
        liftIO $
          atomically newBroadcastTChan
      --
      -- TODO : separate thread to update Bfx.symbolsDetails
      --
      cfg <-
        newTradeConfList bfx $
          rawConfigPairs rc
      this
        Env
          { -- app
            envBfx = bfx,
            envPairs = cfg,
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
    rmLogEnv :: LogEnv -> m ()
    rmLogEnv = void . liftIO . closeScribes
    rmSqlPool :: Pool a -> m ()
    rmSqlPool = liftIO . destroyAllResources

newTradeConfList ::
  ( MonadIO m
  ) =>
  Bfx.Env ->
  Map Bfx.CurrencyPair RawTradeConf ->
  m [MVar TradeConf]
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
  (Bfx.CurrencyPair, RawTradeConf) ->
  m (MVar TradeConf)
newTradeConf symDetails feeDetails (sym, raw) =
  case Map.lookup sym symDetails of
    Nothing -> error $ "Missing " <> show sym
    Just cfg -> do
      let amtNoFee = Bfx.currencyPairMinOrderAmt cfg
      let amtWithFee =
            case Bfx.addFee amtNoFee fee of
              Left e -> error $ show e
              Right x -> x
      liftIO . newMVar $
        TradeConf
          { tradeConfCurrencyPair =
              sym,
            tradeConfCurrencyKind =
              cck,
            tradeConfMinProfitPerOrder =
              rawTradeConfMinProfitPerOrder raw,
            tradeConfMaxQuoteInvestment =
              rawTradeConfMaxQuoteInvestment raw,
            tradeConfBaseFee =
              fee,
            tradeConfQuoteFee =
              coerce fee,
            tradeConfMinBuyAmt =
              amtWithFee,
            tradeConfMinSellAmt =
              coerce amtNoFee,
            tradeConfMode =
              rawTradeConfMode raw
          }
  where
    cck = rawTradeConfCurrencyKind raw
    fee = FeeSummary.getFee @'Bfx.Maker cck feeDetails
