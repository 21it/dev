{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Math
  ( addFee,
    tweakMakerRate,
    newCounterOrder,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import Data.Metrology.Poly ((#))

addFee ::
  Money crel act ->
  FeeRate mrel crel ->
  Money crel act
addFee amt fee =
  Money $
    unMoney amt |/ (1 - unFeeRate fee)

tweakMakerRate ::
  forall act.
  ( SingI act
  ) =>
  QuotePerBase act ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
tweakMakerRate rate@(QuotePerBase rate') =
  tweakMakerRateRec rate rate' tweak
  where
    --
    -- TODO : use pip when 'units' bug with
    -- arithmetic underflow will be fixed.
    -- This implementation is wrong for
    -- non-negative types:
    --
    -- (|-|) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
    -- a |-| b = a |+| qNegate b
    --
    tweak :: Ratio Natural
    tweak =
      case sing :: Sing act of
        SBuy -> 999 % 1000
        SSell -> 1001 % 1000

tweakMakerRateRec ::
  QuotePerBase act ->
  QuotePerBase' ->
  Ratio Natural ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
tweakMakerRateRec rate rate' tweak =
  case roundQuotePerBase
    . from
    $ newRate' # quotePerBaseAmt of
    Left e -> Left e
    Right x | x /= rate -> Right x
    Right {} -> tweakMakerRateRec rate newRate' tweak
  where
    newRate' = rate' |* tweak

newCounterOrder ::
  Money 'Base 'Buy ->
  QuotePerBase 'Buy ->
  FeeRate mrel0 'Base ->
  FeeRate mrel1 'Quote ->
  ProfitRate ->
  ( Money 'Quote 'Sell,
    Money 'Base 'Sell,
    QuotePerBase 'Sell
  )
newCounterOrder base0 rate0 feeB feeQ prof0 =
  ( Money exitQuoteGain,
    Money exitBaseLoss,
    QuotePerBase exitRate
  )
  where
    enterFee :: Ratio Natural
    enterFee =
      from feeB
    exitFee :: Ratio Natural
    exitFee =
      from feeQ
    prof :: Ratio Natural
    prof =
      from prof0
    enterBaseGain :: MoneyBase'
    enterBaseGain =
      unMoney base0
    exitBaseLoss :: MoneyBase'
    exitBaseLoss =
      enterBaseGain |* (1 - enterFee)
    enterQuoteLoss :: MoneyQuote'
    enterQuoteLoss =
      enterBaseGain |*| unQuotePerBase rate0
    exitQuoteGain :: MoneyQuote'
    exitQuoteGain =
      (enterQuoteLoss |* (1 + prof)) |/ (1 - exitFee)
    exitRate :: QuotePerBase'
    exitRate =
      exitQuoteGain |/| exitBaseLoss
