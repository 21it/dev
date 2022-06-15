{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( TradeEnv (..),
    TeleKey (..),
    TeleChat (..),
    TeleEnv (..),
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
import RecklessTradingBot.Data.Time
import RecklessTradingBot.Data.Type
import RecklessTradingBot.Import.External
import qualified Prelude

data TradeEnv = TradeEnv
  { tradeEnvCurrencyPair :: Bfx.CurrencyPair,
    tradeEnvCurrencyKind :: Bfx.CurrencyKind,
    tradeEnvBaseFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Base,
    tradeEnvQuoteFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Quote,
    tradeEnvMinBuyAmt :: Bfx.Money 'Bfx.Base 'Bfx.Buy,
    tradeEnvMinSellAmt :: Bfx.Money 'Bfx.Base 'Bfx.Sell
  }
  deriving stock (Eq)

newtype TeleKey = TeleKey
  { unTeleKey :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      NFData,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance Prelude.Show TeleKey where
  show =
    const "SECRET"

newtype TeleChat = TeleChat
  { unTeleChat :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      NFData,
      FromJSON
    )
  deriving stock
    ( Show,
      Generic
    )

data TeleEnv = TeleEnv
  { teleEnvKey :: TeleKey,
    teleEnvChat :: TeleChat
  }
  deriving stock
    ( Eq,
      Ord,
      Read,
      Show,
      Generic
    )

instance NFData TeleEnv

instance FromJSON TeleEnv where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 7
        }

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envCfg :: MVar (Map Bfx.CurrencyPair TradeEnv),
    envTele :: TeleEnv,
    envPriceTtl :: Seconds,
    envOrderTtl :: Seconds,
    envReportStartAmt :: Bfx.Money 'Bfx.Quote 'Bfx.Sell,
    envReportCurrency :: Bfx.CurrencyCode 'Bfx.Quote,
    envMmaChan :: TChan Bfx.Mma,
    envLastMma :: TMVar Bfx.Mma,
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
    rawEnvTele :: TeleEnv,
    rawEnvPriceTtl :: Seconds,
    rawEnvOrderTtl :: Seconds,
    rawEnvReportStartAmt :: Bfx.Money 'Bfx.Quote 'Bfx.Sell,
    rawEnvReportCurrency :: Bfx.CurrencyCode 'Bfx.Quote,
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
      mmaChan <-
        liftIO $
          atomically newBroadcastTChan
      mmaVar <-
        liftIO $
          atomically newEmptyTMVar
      --
      -- TODO : separate thread to update Bfx.symbolsDetails
      --
      cfg <-
        newTradeCfg bfx
      this
        Env
          { -- app
            envBfx = bfx,
            envCfg = cfg,
            envTele = rawEnvTele rc,
            envPriceTtl = rawEnvPriceTtl rc,
            envOrderTtl = rawEnvOrderTtl rc,
            envReportStartAmt = rawEnvReportStartAmt rc,
            envReportCurrency = rawEnvReportCurrency rc,
            envMmaChan = mmaChan,
            envLastMma = mmaVar,
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

newTradeCfg ::
  ( MonadIO m
  ) =>
  Bfx.Env ->
  m (MVar (Map Bfx.CurrencyPair TradeEnv))
newTradeCfg bfx = do
  ex <-
    runExceptT $
      (,)
        <$> Bfx.symbolsDetails
        <*> Bfx.feeSummary bfx
  case ex of
    Left e -> error $ show e
    Right (symDetails, feeDetails) ->
      newMVar
        . Map.fromList
        $ newTradeCfg' feeDetails <$> Map.assocs symDetails

newTradeCfg' ::
  FeeSummary.Response ->
  (Bfx.CurrencyPair, Bfx.CurrencyPairConf) ->
  (Bfx.CurrencyPair, TradeEnv)
newTradeCfg' feeDetails (sym, cfg) =
  ( sym,
    TradeEnv
      { tradeEnvCurrencyPair =
          sym,
        tradeEnvCurrencyKind =
          cck,
        tradeEnvBaseFee =
          fee,
        tradeEnvQuoteFee =
          coerce fee,
        tradeEnvMinBuyAmt =
          amtWithFee,
        tradeEnvMinSellAmt =
          coerce amtNoFee
      }
  )
  where
    --
    -- TODO : fixme, unhardcode cck!!!
    --
    cck = Bfx.Fiat
    fee = FeeSummary.getFee @'Bfx.Maker cck feeDetails
    amtNoFee = Bfx.currencyPairMinOrderAmt cfg
    amtWithFee =
      case Bfx.tweakMoneyPip
        =<< Bfx.addFee amtNoFee fee of
        Left e -> error $ show e
        Right x -> x
