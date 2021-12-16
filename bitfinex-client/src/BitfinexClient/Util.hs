{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Util
  ( absRat,
    bfxRoundRatio,
    eradicateNull,
    readVia,
    tryReadVia,
    readViaRatio,
    tryReadViaRatio,
  )
where

import BitfinexClient.Import.External
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import GHC.Natural (naturalFromInteger)

absRat :: Rational -> Ratio Natural
absRat x =
  (naturalFromInteger . abs $ numerator x)
    % (naturalFromInteger . abs $ denominator x)

bfxRoundRatio :: (From a Rational) => a -> Rational
bfxRoundRatio =
  sdRound 5
    . dpRound 8
    . from

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

readVia ::
  forall through target source.
  ( Read through,
    From through target,
    ToString source
  ) =>
  source ->
  Either (TryFromException source target) target
readVia x0 =
  case readMaybe . fromString $ toString x0 of
    Nothing -> Left $ TryFromException x0 Nothing
    Just x -> Right $ from @through x

tryReadVia ::
  forall through target source.
  ( Read through,
    TryFrom through target,
    ToString source
  ) =>
  source ->
  Either (TryFromException source target) target
tryReadVia x0 =
  case readMaybe . fromString $ toString x0 of
    Nothing -> Left $ TryFromException x0 Nothing
    Just x -> first (withSource x0) $ tryFrom @through x

readViaRatio ::
  forall through target source.
  ( Fractional through,
    From through target,
    ToString source
  ) =>
  source ->
  Either (TryFromException source target) target
readViaRatio x0 =
  case T.rational . T.strip . fromString $ toString x0 of
    Right (x, "") -> Right $ from @through x
    Right {} -> failure
    Left {} -> failure
  where
    failure = Left $ TryFromException x0 Nothing

tryReadViaRatio ::
  forall through target source.
  ( Fractional through,
    TryFrom through target,
    ToString source
  ) =>
  source ->
  Either (TryFromException source target) target
tryReadViaRatio x0 =
  case T.rational . T.strip . T.pack $ toString x0 of
    Right (x, "") -> first (withSource x0) $ tryFrom @through x
    Right {} -> failure
    Left {} -> failure
  where
    failure = Left $ TryFromException x0 Nothing
