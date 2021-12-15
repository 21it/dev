{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Math
  ( applyFee,
    tweakMakerRate,
    newCounterOrder,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

applyFee ::
  MoneyAmt dim act ->
  FeeRate a b ->
  MoneyAmt dim act
applyFee amt fee =
  MoneyAmt $
    unMoneyAmt amt |* unFeeRate fee

tweakMakerRate ::
  forall act.
  ( SingI act
  ) =>
  QuotePerBase act ->
  QuotePerBase act
tweakMakerRate (QuotePerBase rate) =
  QuotePerBase $ rate |* tweak
  where
    tweak :: Ratio Natural
    tweak =
      case sing :: Sing act of
        SBuy -> 999 % 1000
        SSell -> 1001 % 1000

newCounterOrder ::
  MoneyBase 'Buy ->
  QuotePerBase 'Buy ->
  FeeRate a b ->
  ProfitRate ->
  ( MoneyBase 'Sell,
    QuotePerBase 'Sell
  )
newCounterOrder base0 rate0 fee0 prof0 =
  ( MoneyAmt exitBaseLoss,
    QuotePerBase exitRate
  )
  where
    fee :: Ratio Natural
    fee =
      from fee0
    prof :: Ratio Natural
    prof =
      from prof0
    enterBaseGain :: MoneyBase'
    enterBaseGain =
      unMoneyAmt base0
    exitBaseLoss :: MoneyBase'
    exitBaseLoss =
      enterBaseGain |* (1 - fee)
    enterQuoteLoss :: MoneyQuote'
    enterQuoteLoss =
      enterBaseGain |*| unQuotePerBase rate0
    exitQuoteGain :: MoneyQuote'
    exitQuoteGain =
      (enterQuoteLoss |* (1 + prof)) |/ (1 - fee)
    exitRate :: QuotePerBase'
    exitRate =
      exitQuoteGain |/| exitBaseLoss
