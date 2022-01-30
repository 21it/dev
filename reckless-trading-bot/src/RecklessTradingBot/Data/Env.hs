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
    header,
    help,
    keep,
    nonempty,
    parse,
    var,
  )
import RecklessTradingBot.Data.Model
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External
import qualified Prelude

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
    rawEnvBfx :: Bfx.RawEnv,
    rawEnvPairs :: Map Bfx.CurrencyPair RawTradeEnv,
    rawEnvPriceTtl :: Seconds,
    rawEnvOrderTtl :: Seconds,
    -- storage
    rawEnvLibpqConnStr :: LibpqConnStr,
    -- logging
    rawEnvLogEnv :: Text,
    rawEnvLogFormat :: LogFormat,
    rawEnvLogSeverity :: Severity,
    rawEnvLogVerbosity :: Verbosity
  }
  deriving stock
    ( Eq,
      Show,
      Generic
    )

instance FromJSON RawEnv where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 6
        }

newtype LibpqConnStr
  = LibpqConnStr ConnectionString
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance Prelude.Show LibpqConnStr where
  show =
    const "SECRET"

instance From ConnectionString LibpqConnStr

instance From LibpqConnStr ConnectionString

instance FromJSON LibpqConnStr where
  parseJSON =
    Bfx.parseJsonBs

sysRawConfig :: (MonadUnliftIO m) => m RawEnv
sysRawConfig = liftIO $ do
  parse (header "RecklessTradingBot") $
    var
      (json <=< nonempty)
      "RECKLESS_TRADING_BOT_ENV"
      (keep <> help "")
  where
    json ::
      String ->
      Either Env.Error RawEnv
    json =
      first
        UnreadError
        . A.eitherDecodeStrict
        . C8.pack

withEnv :: forall m. (MonadUnliftIO m) => (Env -> m ()) -> m ()
withEnv this = do
  rc <- sysRawConfig
  let sev = rawEnvLogSeverity rc
  let connStr = from $ rawEnvLibpqConnStr rc
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
      bfx <- Bfx.newEnv $ rawEnvBfx rc
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
