{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Util
  ( eradicateNull,
    fromRatio,
    mapRatio,
  )
where

import BitfinexClient.Import.External
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HS
import qualified Data.Vector as V

eradicateNull :: A.Value -> A.Value
eradicateNull = \case
  A.Object xs -> A.Object $ HS.mapMaybe devastateNull xs
  A.Array xs -> A.Array $ V.mapMaybe devastateNull xs
  x -> x
  where
    devastateNull =
      \case
        A.Null -> Nothing
        x -> Just $ eradicateNull x

fromRatio :: forall a b. (From a b, Integral b) => Ratio a -> Ratio b
fromRatio x = from @a @b (numerator x) % from @a @b (denominator x)

mapRatio :: Integral b => (a -> b) -> Ratio a -> Ratio b
mapRatio f x = f (numerator x) % f (denominator x)
