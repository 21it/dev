{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Model.Order
  ( create,
    updateStatus,
    updateBfx,
    getByStatus,
  )
where

import qualified BitfinexClient as Bfx
import qualified Database.Persist as P
import RecklessTradingBot.Class.Storage
import RecklessTradingBot.Import
import qualified RecklessTradingBot.Import.Psql as Psql

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

updateStatus ::
  ( Storage m
  ) =>
  OrderStatus ->
  [OrderId] ->
  m ()
updateStatus ss xs =
  runSql $
    Psql.update $ \row -> do
      Psql.set row [OrderStatus Psql.=. Psql.val ss]
      Psql.where_ $
        row Psql.^. OrderId `Psql.in_` Psql.valList xs

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

getByStatus ::
  ( Storage m
  ) =>
  Bfx.CurrencyPair ->
  [OrderStatus] ->
  m [Entity Order]
getByStatus sym ss =
  runSql $
    P.selectList
      [ OrderBase
          P.==. Bfx.currencyPairBase sym,
        OrderQuote
          P.==. Bfx.currencyPairQuote sym,
        OrderStatus
          P.<-. ss
      ]
      [ P.LimitTo 100
      ]
