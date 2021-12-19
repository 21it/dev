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
import Control.Monad.Logger (runNoLoggingT)
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
  { rawTradeConfCurrencyKind :: Bfx.CurrencyKind,
    rawTradeConfProfitPerOrder :: Bfx.ProfitRate,
    rawTradeConfMaxQuoteInvestment :: Bfx.MoneyQuote 'Bfx.Sell
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
  { tradingConfPair :: Bfx.CurrencyPair,
    tradingConfFee :: Bfx.FeeRate 'Bfx.Maker 'Bfx.Base,
    tradingConfMinOrderAmt :: MVar (Bfx.MoneyBase 'Bfx.Buy)
  }
  deriving stock (Eq)

data Env = Env
  { -- app
    envBfx :: Bfx.Env,
    envPairs :: [TradeConf],
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
        ( err . tryFrom @(Ratio Natural)
            <=< auto
            <=< nonempty
        )
        "RECKLESS_TRADING_BOT_PROFIT"
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
        "RECKLESS_TRADING_BOT_LOG_VERBOSITY"
        op
  where
    op :: Mod Var a
    op = keep <> help ""
    json ::
      String ->
      Either Env.Error (Set Bfx.CurrencyPair)
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

withEnv :: forall m. (MonadUnliftIO m) => (Env -> m ()) -> m ()
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
    rmLogEnv :: LogEnv -> m ()
    rmLogEnv = void . liftIO . closeScribes
    rmSqlPool :: Pool a -> m ()
    rmSqlPool = liftIO . destroyAllResources

newTradingConfSet ::
  MonadIO m =>
  Bfx.Env ->
  Set Bfx.CurrencyPair ->
  m [TradeConf]
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
  m TradeConf
newTradingConf symDetails feeDetails sym =
  case Map.lookup sym symDetails of
    Nothing -> error $ "Missing " <> show sym
    Just cfg -> do
      let amtDef = Bfx.currencyPairMinOrderAmt cfg
      let amtInclFee = Bfx.applyFee amtDef fee
      when (amtInclFee <= from @(Ratio Natural) 0)
        . error
        $ "Wrong min Order " <> show amtInclFee
      amtVar <-
        liftIO . newMVar $ from amtInclFee
      pure $
        TradeConf
          { tradingConfPair = sym,
            tradingConfFee = fee,
            tradingConfMinOrderAmt = amtVar
          }
  where
    fee = FeeSummary.makerCrypto2CryptoFee feeDetails
