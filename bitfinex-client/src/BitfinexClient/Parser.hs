{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Parser
  ( parseOrder,
    parseOrderMap,
  )
where

import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import Data.Aeson.Lens
import qualified Data.Map as Map

parseOrder :: AsValue a => a -> Either Text (Order b)
parseOrder x = do
  id0 <-
    maybeToRight "OrderId is missing" $
      OrderId
        <$> x ^? nth 0 . _Integral
  gid <-
    maybeToRight "OrderGroupId is missing" $
      (Just . OrderGroupId <$> x ^? nth 1 . _Integral)
        <|> (Nothing <$ x ^? nth 1 . _Null)
  cid <-
    maybeToRight "OrderClientId is missing" $
      (Just . OrderClientId <$> x ^? nth 2 . _Integral)
        <|> (Nothing <$ x ^? nth 2 . _Null)
  sym0 <-
    maybeToRight "Symbol is missing" $
      x ^? nth 3 . _String
  sym <-
    first (const $ "Symbol is invalid " <> sym0) $
      newCurrencyPair' sym0
  amt0 <-
    maybeToRight "OrderAmount is missing" $
      toRational <$> x ^? nth 7 . _Number
  amt <-
    first (const $ "OrderAmount is invalid " <> show amt0) $
      newMoneyAmount $ abs amt0
  act <-
    first (const $ "OrderAmount is invalid " <> show amt0) $
      newExchangeAction amt0
  ss0 <-
    maybeToRight "OrderStatus is missing" $
      x ^? nth 13 . _String
  ss1 <-
    newOrderStatus ss0
  price <-
    maybeToRight
      "ExchangeRate is missing"
      $ x ^? nth 16 . _Number
  rate <-
    first (const $ "ExchangeRate is invalid " <> show price)
      . newExchangeRate
      $ toRational price
  pure
    Order
      { orderId = id0,
        orderGroupId = gid,
        orderClientId = cid,
        orderAction = act,
        orderAmount = amt,
        orderSymbol = sym,
        orderRate = rate,
        orderStatus = ss1
      }

parseOrderMap :: AsValue a => a -> Either Text (Map OrderId (Order b))
parseOrderMap raw = do
  xs <-
    maybeToRight
      "Json is not an Array"
      $ raw ^? _Array
  foldrM parser mempty xs
  where
    parser x acc = do
      order <- parseOrder x
      pure $ Map.insert (orderId order) order acc
