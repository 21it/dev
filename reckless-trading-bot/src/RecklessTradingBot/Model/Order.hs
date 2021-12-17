{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Order
  ( create,
    updateBfx,
    getOngoing,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import

create ::
  ( Storage m
  ) =>
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
          orderGain = from @(Ratio Natural) 0,
          orderLoss = from @(Ratio Natural) 0,
          orderFee = [Bfx.feeRateMakerBase| 0 |],
          orderStatus = OrderNew,
          orderAt = ct
        }

updateBfx ::
  ( Storage m
  ) =>
  OrderId ->
  Bfx.SomeOrder 'Bfx.Remote ->
  m OrderStatus
updateBfx rowId (Bfx.SomeOrder bfxS bfxOrder) = runSql $ do
  P.update rowId $
    --
    -- TODO : FIXME !!!
    --
    case bfxS of
      Bfx.SBuy ->
        [ OrderExtRef P.=. Just (from $ Bfx.orderId bfxOrder),
          OrderPrice P.=. Bfx.orderRate bfxOrder,
          OrderStatus P.=. ss
        ]
      Bfx.SSell ->
        [ OrderExtRef P.=. Just (from $ Bfx.orderId bfxOrder),
          OrderStatus P.=. ss
        ]
  pure ss
  where
    ss :: OrderStatus
    ss = from $ Bfx.orderStatus bfxOrder

getOngoing ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  m [Entity Order]
getOngoing sym =
  runSql $
    P.selectList
      [ OrderBase
          P.==. Bfx.currencyPairBase sym,
        OrderQuote
          P.==. Bfx.currencyPairQuote sym,
        OrderStatus
          P.<-. [OrderNew, OrderActive]
      ]
      [ P.LimitTo 100
      ]
