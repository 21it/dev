{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Order
  ( getOngoing,
    create,
    updateBfx,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import

create ::
  Storage m =>
  Entity Price ->
  m (Entity Order)
create (Entity priceId price) = do
  row <- liftIO $ newOrder <$> getCurrentTime
  rowId <- runSql $ P.insert row
  pure $ Entity rowId row
  where
    newOrder ct =
      Order
        { orderPriceRef = priceId,
          orderBase = priceBase price,
          orderQuote = priceQuote price,
          orderIntRef = Nothing,
          orderExtRef = Nothing,
          orderPrice = priceBuy price,
          --
          -- TODO : !!!
          --
          orderGain = 0,
          orderLoss = 0,
          orderFee = 0,
          orderStatus = OrderNew,
          orderAt = ct
        }

updateBfx ::
  (Storage m) =>
  OrderId ->
  Bfx.Order 'Bfx.Remote ->
  m OrderStatus
updateBfx rowId bfxOrder = runSql $ do
  P.update
    rowId
    --
    -- TODO : !!!
    --
    [ OrderExtRef P.=. Just extRef,
      OrderPrice P.=. rate,
      OrderStatus P.=. ss
    ]
  pure ss
  where
    extRef = from $ Bfx.orderId bfxOrder
    rate = from $ Bfx.orderRate bfxOrder
    ss = from $ Bfx.orderStatus bfxOrder

getOngoing ::
  Storage m =>
  Bfx.CurrencyPair ->
  m [Entity Order]
getOngoing sym =
  runSql $
    P.selectList
      [ OrderBase
          P.==. CurrencyCode
            (Bfx.currencyPairBase sym),
        OrderQuote
          P.==. CurrencyCode
            (Bfx.currencyPairQuote sym),
        OrderStatus
          P.<-. [OrderNew, OrderActive]
      ]
      [ P.LimitTo 100
      ]
