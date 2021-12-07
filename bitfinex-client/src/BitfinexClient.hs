{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient
  ( symbolsDetails,
    marketAveragePrice,
    feeSummary,
    wallets,
    spendableExchangeBalance,
    retrieveOrders,
    ordersHistory,
    getOrders,
    getOrder,
    verifyOrder,
    submitOrder,
    submitOrderMaker,
    cancelOrderMulti,
    cancelOrderById,
    cancelOrderByClientId,
    cancelOrderByGroupId,
    submitCounterOrder,
    submitCounterOrderMaker,
    dumpIntoQuote,
    dumpIntoQuoteMaker,
    module X,
  )
where

import qualified BitfinexClient.Data.CancelOrderMulti as CancelOrderMulti
import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import qualified BitfinexClient.Data.Wallets as Wallets
import BitfinexClient.Import
import qualified BitfinexClient.Import.Internal as X
import qualified BitfinexClient.Math as Math
import qualified BitfinexClient.Rpc.Generic as Generic
import qualified Data.Map as Map
import qualified Data.Set as Set

symbolsDetails ::
  ( MonadIO m
  ) =>
  ExceptT Error m (Map CurrencyPair CurrencyPairConf)
symbolsDetails =
  Generic.pub (Generic.Rpc :: Generic.Rpc 'SymbolsDetails) [] ()

marketAveragePrice ::
  ( MonadIO m
  ) =>
  ExchangeAction ->
  MoneyBase ->
  CurrencyPair ->
  ExceptT Error m QuotePerBase
marketAveragePrice act amt sym =
  Generic.pub
    (Generic.Rpc :: Generic.Rpc 'MarketAveragePrice)
    [ SomeQueryParam "amount" (act, amt),
      SomeQueryParam "symbol" sym
    ]
    MarketAveragePrice.Request
      { MarketAveragePrice.action = act,
        MarketAveragePrice.amount = amt,
        MarketAveragePrice.symbol = sym
      }

feeSummary ::
  ( MonadIO m
  ) =>
  Env ->
  ExceptT Error m FeeSummary.Response
feeSummary env =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'FeeSummary)
    env
    (mempty :: Map Int Int)

wallets ::
  ( MonadIO m
  ) =>
  Env ->
  ExceptT
    Error
    m
    ( Map
        (CurrencyCode 'Base)
        ( Map
            Wallets.WalletType
            Wallets.Response
        )
    )
wallets env =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'Wallets)
    env
    (mempty :: Map Int Int)

spendableExchangeBalance ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyCode 'Base ->
  ExceptT Error m MoneyBase
spendableExchangeBalance env cc =
  --
  -- TODO : implement QQ for Money amounts
  --
  maybe (from @(Ratio Natural) 0) Wallets.availableBalance
    . Map.lookup Wallets.Exchange
    . Map.findWithDefault mempty cc
    <$> wallets env

retrieveOrders ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
retrieveOrders =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'RetrieveOrders)

ordersHistory ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
ordersHistory =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'OrdersHistory)

getOrders ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
getOrders env opts = do
  xs0 <- retrieveOrders env opts
  xs1 <- ordersHistory env opts
  pure $ xs1 <> xs0

getOrder ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  ExceptT Error m (Order 'Remote)
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  except $ maybeToRight (ErrorMissingOrder id0) mOrder

verifyOrder ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  SubmitOrder.Request ->
  ExceptT Error m (Order 'Remote)
verifyOrder env id0 req = do
  remOrd <- getOrder env id0
  let locOrd =
        Order
          { orderId = id0,
            orderGroupId = SubmitOrder.groupId opts,
            orderClientId =
              SubmitOrder.clientId opts <|> orderClientId remOrd,
            orderAction = SubmitOrder.action req,
            orderAmount = SubmitOrder.amount req,
            orderSymbol = SubmitOrder.symbol req,
            orderRate = SubmitOrder.rate req,
            orderStatus = orderStatus remOrd
          }
  if remOrd == locOrd
    then pure remOrd
    else throwE $ ErrorUnverifiedOrder (coerce locOrd) remOrd
  where
    opts = SubmitOrder.options req

submitOrder ::
  ( MonadIO m
  ) =>
  Env ->
  ExchangeAction ->
  MoneyBase ->
  CurrencyPair ->
  QuotePerBase ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitOrder env act amt sym rate opts = do
  order :: Order 'Remote <-
    Generic.prv (Generic.Rpc :: Generic.Rpc 'SubmitOrder) env req
  verifyOrder env (orderId order) req
  where
    req =
      SubmitOrder.Request
        { SubmitOrder.action = act,
          SubmitOrder.amount = amt,
          SubmitOrder.symbol = sym,
          SubmitOrder.rate = rate,
          SubmitOrder.options = opts
        }

submitOrderMaker ::
  forall m.
  ( MonadIO m
  ) =>
  Env ->
  ExchangeAction ->
  MoneyBase ->
  CurrencyPair ->
  QuotePerBase ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitOrderMaker env act amt sym rate0 opts0 =
  this 0 rate0
  where
    opts =
      opts0
        { SubmitOrder.flags =
            Set.insert PostOnly $
              SubmitOrder.flags opts0
        }
    this ::
      Int ->
      QuotePerBase ->
      ExceptT Error m (Order 'Remote)
    this attempt rate = do
      order <- submitOrder env act amt sym rate opts
      if orderStatus order /= PostOnlyCanceled
        then pure order
        else do
          when (attempt >= 10) $ throwE $ ErrorOrderState order
          newRate <-
            tryFromT
              . bfxRoundRatio
              . into @(Ratio Natural)
              $ Math.tweakMakerRate act rate
          this (attempt + 1) newRate

cancelOrderMulti ::
  ( MonadIO m
  ) =>
  Env ->
  CancelOrderMulti.Request ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderMulti =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'CancelOrderMulti)

cancelOrderById ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  ExceptT Error m (Order 'Remote)
cancelOrderById env id0 = do
  mOrder <-
    Map.lookup id0
      <$> cancelOrderMulti
        env
        ( CancelOrderMulti.ByOrderId $ Set.singleton id0
        )
  except $
    maybeToRight (ErrorMissingOrder id0) mOrder

cancelOrderByClientId ::
  ( MonadIO m
  ) =>
  Env ->
  OrderClientId ->
  UTCTime ->
  ExceptT Error m (Maybe (Order 'Remote))
cancelOrderByClientId env cid utc =
  listToMaybe . elems
    <$> cancelOrderMulti
      env
      ( CancelOrderMulti.ByOrderClientId $
          Set.singleton (cid, utc)
      )

cancelOrderByGroupId ::
  ( MonadIO m
  ) =>
  Env ->
  OrderGroupId ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderByGroupId env gid = do
  cancelOrderMulti env . CancelOrderMulti.ByOrderGroupId $
    Set.singleton gid

submitCounterOrder ::
  forall a b m.
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  FeeRate a b ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrder =
  submitCounterOrder' submitOrder

submitCounterOrderMaker ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  FeeRate 'Maker 'Quote ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrderMaker =
  submitCounterOrder' submitOrderMaker

submitCounterOrder' ::
  ( MonadIO m
  ) =>
  ( Env ->
    ExchangeAction ->
    MoneyBase ->
    CurrencyPair ->
    QuotePerBase ->
    SubmitOrder.Options ->
    ExceptT Error m (Order 'Remote)
  ) ->
  Env ->
  OrderId ->
  FeeRate a b ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrder' submit env id0 fee prof opts = do
  order <- getOrder env id0
  let (exitAmt, exitRate) =
        Math.newCounterOrder
          (orderAmount order)
          (orderRate order)
          fee
          prof
  if orderAction order == Buy && orderStatus order == Executed
    then submit env Sell exitAmt (orderSymbol order) exitRate opts
    else throwE $ ErrorOrderState order

dumpIntoQuote' ::
  ( MonadIO m
  ) =>
  ( Env ->
    ExchangeAction ->
    MoneyBase ->
    CurrencyPair ->
    QuotePerBase ->
    SubmitOrder.Options ->
    ExceptT Error m (Order 'Remote)
  ) ->
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
dumpIntoQuote' submit env sym opts = do
  amt <- spendableExchangeBalance env $ currencyPairBase sym
  rate <- marketAveragePrice Sell amt sym
  submit env Sell amt sym rate opts

dumpIntoQuote ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
dumpIntoQuote =
  dumpIntoQuote' submitOrder

dumpIntoQuoteMaker ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
dumpIntoQuoteMaker =
  dumpIntoQuote' submitOrderMaker
