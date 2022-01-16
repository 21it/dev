{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Parser
  ( parseOrder,
    parseOrderMap,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import BitfinexClient.Util
import Data.Aeson.Lens
import qualified Data.Map as Map

parseOrder ::
  ( AsValue a,
    Show a
  ) =>
  a ->
  Either Text (SomeOrder 'Remote)
parseOrder x = do
  id0 <-
    maybeToRight (failure "OrderId is missing") $
      OrderId
        <$> x ^? nth 0 . _Integral
  gid <-
    maybeToRight (failure "OrderGroupId is missing") $
      (Just . OrderGroupId <$> x ^? nth 1 . _Integral)
        <|> ( ( either
                  (const Nothing)
                  (Just . Just)
                  . readVia @Natural @OrderGroupId @Text
              )
                =<< x ^? nth 1 . _String
            )
        <|> (Nothing <$ x ^? nth 1 . _Null)
  cid <-
    maybeToRight (failure "OrderClientId is missing") $
      (Just . OrderClientId <$> x ^? nth 2 . _Integral)
        <|> (Nothing <$ x ^? nth 2 . _Null)
  sym0 <-
    maybeToRight (failure "Symbol is missing") $
      x ^? nth 3 . _String
  sym <-
    first (const $ failure "Symbol is invalid") $
      newCurrencyPair sym0
  amt0 <-
    maybeToRight (failure "OrderAmount is missing") $
      toRational <$> x ^? nth 7 . _Number
  SomeMoney sAct amt <-
    first (failure . ("OrderAmount is invalid " <>) . show) $
      tryFrom amt0
  ss0 <-
    maybeToRight (failure "OrderStatus is missing") $
      x ^? nth 13 . _String
  ss1 <-
    first
      ( failure
          . ("OrderStatus is not recognized " <>)
          . show
      )
      $ newOrderStatus ss0
  price <-
    maybeToRight
      (failure "ExchangeRate is missing")
      $ x ^? nth 16 . _Number
  rate <-
    first
      ( const
          . failure
          $ "ExchangeRate is invalid " <> show price
      )
      $ tryFrom (toRational price)
  pure . SomeOrder sAct $
    Order
      { orderId = id0,
        orderGroupId = gid,
        orderClientId = cid,
        orderAmount = amt,
        orderSymbol = sym,
        orderRate = rate,
        orderStatus = ss1
      }
  where
    failure =
      (<> " in " <> show x)

parseOrderMap ::
  ( AsValue a
  ) =>
  a ->
  Either Text (Map OrderId (SomeOrder 'Remote))
parseOrderMap raw = do
  xs <-
    maybeToRight
      "Json is not an Array"
      $ raw ^? _Array
  foldrM parser mempty xs
  where
    parser x acc = do
      someOrder@(SomeOrder _ order) <- parseOrder x
      pure $ Map.insert (orderId order) someOrder acc
