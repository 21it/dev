{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient
  ( marketAveragePrice,
    feeSummary,
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
    module X,
  )
where

import qualified BitfinexClient.Data.CancelOrderMulti as CancelOrderMulti
import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import qualified BitfinexClient.Import.Internal as X
import qualified BitfinexClient.Rpc.Generic as Generic
import qualified Data.Map as Map
import qualified Data.Set as Set

marketAveragePrice ::
  MonadIO m =>
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExceptT Error m ExchangeRate
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
  MonadIO m =>
  Env ->
  ExceptT Error m FeeSummary.Response
feeSummary env =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'FeeSummary)
    env
    (mempty :: Map Int Int)

retrieveOrders ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
retrieveOrders =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'RetrieveOrders)

ordersHistory ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
ordersHistory =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'OrdersHistory)

getOrders ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
getOrders env opts = do
  xs0 <- retrieveOrders env opts
  xs1 <- ordersHistory env opts
  pure $ xs1 <> xs0

getOrder ::
  MonadIO m =>
  Env ->
  OrderId ->
  ExceptT Error m (Order 'Remote)
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  except $ maybeToRight (ErrorMissingOrder id0) mOrder

verifyOrder ::
  MonadIO m =>
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
  MonadIO m =>
  Env ->
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExchangeRate ->
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
  MonadIO m =>
  Env ->
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExchangeRate ->
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
    tweak0 =
      case act of
        Buy -> 999 % 1000
        Sell -> 1001 % 1000
    this :: Int -> ExchangeRate -> ExceptT Error m (Order 'Remote)
    this attempt rate = do
      order <- submitOrder env act amt sym rate opts
      if orderStatus order /= PostOnlyCanceled
        then pure order
        else do
          when (attempt >= 10) $ throwE $ ErrorOrderState order
          tweak <- except $ newExchangeRate tweak0
          this (attempt + 1) . bfxRoundPosRat $ tweak * rate

cancelOrderMulti ::
  MonadIO m =>
  Env ->
  CancelOrderMulti.Request ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderMulti =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'CancelOrderMulti)

cancelOrderById ::
  MonadIO m =>
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
  MonadIO m =>
  Env ->
  OrderClientId ->
  UTCTime ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderByClientId env cid utc =
  cancelOrderMulti env . CancelOrderMulti.ByOrderClientId $
    Set.singleton (cid, utc)

cancelOrderByGroupId ::
  MonadIO m =>
  Env ->
  OrderGroupId ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderByGroupId env gid = do
  cancelOrderMulti env . CancelOrderMulti.ByOrderGroupId $
    Set.singleton gid

submitCounterOrder ::
  MonadIO m =>
  Env ->
  OrderId ->
  FeeRate a ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrder =
  submitCounterOrder' submitOrder

submitCounterOrderMaker ::
  MonadIO m =>
  Env ->
  OrderId ->
  FeeRate 'Maker ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrderMaker =
  submitCounterOrder' submitOrderMaker

submitCounterOrder' ::
  MonadIO m =>
  ( Env ->
    ExchangeAction ->
    MoneyAmount ->
    CurrencyPair ->
    ExchangeRate ->
    SubmitOrder.Options ->
    ExceptT Error m (Order 'Remote)
  ) ->
  Env ->
  OrderId ->
  FeeRate a ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrder' submit env id0 feeRate profRate opts = do
  order <- getOrder env id0
  let amtOrder = orderAmount order
  let amtQuoteLoss = amtOrder * coerce (orderRate order)
  --
  -- TODO : improve math there, seems not 100% accurate
  --
  let amtQuoteGain = amtQuoteLoss * (1 + coerce feeRate + coerce profRate)
  amtBase <-
    except $
      bfxRoundPosRat . (amtOrder *) . coerce
        <$> (1 `subPosRat` coerce feeRate)
  let sellCalcPrice = coerce $ amtQuoteGain / amtBase
  if orderAction order == Buy && orderStatus order == Executed
    then do
      let sym = orderSymbol order
      sellAvgPrice <- marketAveragePrice Sell amtBase sym
      let sellPrice = bfxRoundPosRat $ max sellCalcPrice sellAvgPrice
      submit env Sell amtBase sym sellPrice opts
    else throwE $ ErrorOrderState order
