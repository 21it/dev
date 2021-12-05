{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.QQ
  ( posRat,
    --feeRate,
    feeRateMakerBase,
    feeRateMakerQuote,
    feeRateTakerBase,
    feeRateTakerQuote,
    ccBase,
    ccQuote,
    currencyPair,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import BitfinexClient.Util
import qualified Data.Text as T
import Data.Typeable (typeRep)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax as TH (Lift)

showError ::
  forall source target.
  ( Show source,
    Typeable target
  ) =>
  Either (TryFromException source target) target ->
  Either String target
showError =
  first $ \(TryFromException x0 _) ->
    show (typeRep $ Proxy @target)
      <> " can not be read from "
      <> show x0

mkQQ ::
  forall target.
  ( TH.Lift target,
    Typeable target
  ) =>
  (String -> Either String target) ->
  QuasiQuoter
mkQQ parser =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \x0 ->
          case parser x0 of
            Left e -> fail e
            Right x -> [|x|]
    }
  where
    failure field =
      error $
        show (typeRep $ Proxy @target)
          <> " "
          <> field
          <> " is not implemented"

mkTryRatQQ ::
  forall through target.
  ( Read through,
    Fractional through,
    TryFrom through target,
    TH.Lift target,
    Typeable target
  ) =>
  QuasiQuoter
mkTryRatQQ =
  mkQQ $ \x0 ->
    showError (tryReadViaRatio @through @target x0)
      <|> showError (tryReadVia @through @target x0)

posRat :: QuasiQuoter
posRat =
  mkTryRatQQ @(Ratio Natural) @PosRat

--
-- TODO : for some reason this is not working:
-- No instance for (Typeable mr0) arising from a use of ‘feeRate’
--
-- feeRate ::
--   forall (mr :: MarketRelation) (cr :: CurrencyRelation).
--   ( Typeable mr,
--     Typeable cr
--   ) =>
--   QuasiQuoter
-- feeRate =
--   mkTryRatQQ @(Ratio Natural) @(FeeRate mr cr)

feeRateMakerBase :: QuasiQuoter
feeRateMakerBase =
  mkTryRatQQ @(Ratio Natural) @(FeeRate 'Maker 'Base)

feeRateMakerQuote :: QuasiQuoter
feeRateMakerQuote =
  mkTryRatQQ @(Ratio Natural) @(FeeRate 'Maker 'Quote)

feeRateTakerBase :: QuasiQuoter
feeRateTakerBase =
  mkTryRatQQ @(Ratio Natural) @(FeeRate 'Taker 'Base)

feeRateTakerQuote :: QuasiQuoter
feeRateTakerQuote =
  mkTryRatQQ @(Ratio Natural) @(FeeRate 'Taker 'Quote)

ccBase :: QuasiQuoter
ccBase =
  mkQQ $
    showError
      . newCurrencyCode @'Base
      . T.pack

ccQuote :: QuasiQuoter
ccQuote =
  mkQQ $
    showError
      . newCurrencyCode @'Quote
      . T.pack

currencyPair :: QuasiQuoter
currencyPair =
  mkQQ $
    showError
      . newCurrencyPair'
      . T.strip
      . T.pack
