{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Data.Env
  ( TradeEnv (..),
    newTradeEnv,
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
    envTele :: TeleEnv,
    envTrade :: MVar (Map Bfx.CurrencyPair TradeEnv),
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
        newTradeVar bfx
      this
        Env
          { -- app
            envBfx = bfx,
            envTele = rawEnvTele rc,
            envTrade = cfg,
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

newTradeVar ::
  ( MonadIO m
  ) =>
  Bfx.Env ->
  m (MVar (Map Bfx.CurrencyPair TradeEnv))
newTradeVar bfx = do
  ex <-
    runExceptT $ do
      fee <- Bfx.feeSummary bfx
      sym <- Bfx.symbolsDetails
      except $ newTradeEnv fee sym
  case ex of
    Left e -> error $ "Fatal TradeEnv failure " <> show e
    Right x -> newMVar x

newTradeEnv ::
  FeeSummary.Response ->
  Map Bfx.CurrencyPair Bfx.CurrencyPairConf ->
  Either Bfx.Error (Map Bfx.CurrencyPair TradeEnv)
newTradeEnv feeDetails symDetails = do
  xs <-
    mapM (uncurry $ newTradeEnvEntry feeDetails) $
      Map.assocs symDetails
  pure $
    Map.fromList xs

newTradeEnvEntry ::
  FeeSummary.Response ->
  Bfx.CurrencyPair ->
  Bfx.CurrencyPairConf ->
  Either Bfx.Error (Bfx.CurrencyPair, TradeEnv)
newTradeEnvEntry feeDetails sym cfg = do
  --
  -- TODO : fixme, unhardcode cck!!!
  --
  let cck = Bfx.Stable
  let fee = FeeSummary.getFee @'Bfx.Maker cck feeDetails
  let amtNoFee = Bfx.currencyPairMinOrderAmt cfg
  when (amtNoFee <= [moneyBaseBuy|0|])
    . Left
    . Bfx.ErrorMath
    $ "Wrong min order for sym = "
      <> show sym
      <> " and cfg = "
      <> show cfg
  amtWithFee <-
    first (Bfx.ErrorTryFrom . SomeException) $
      Bfx.tweakMoneyPip
        =<< Bfx.addFee amtNoFee fee
  pure
    ( sym,
      TradeEnv
        { tradeEnvCurrencyPair = sym,
          tradeEnvCurrencyKind = cck,
          tradeEnvBaseFee = fee,
          tradeEnvQuoteFee = coerce fee,
          tradeEnvMinBuyAmt = amtWithFee,
          tradeEnvMinSellAmt = coerce amtNoFee
        }
    )
