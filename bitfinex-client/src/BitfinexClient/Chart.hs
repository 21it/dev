{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Chart
  ( newExample,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.Candles as Candles
import BitfinexClient.Import
import qualified BitfinexClient.Indicator.Ma as Ma
import qualified BitfinexClient.Indicator.Mma as Mma
import qualified Data.Map as Map
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG as SVG

newExample :: (MonadIO m) => m ()
newExample = do
  ecs <-
    runExceptT $
      Bfx.candlesHist
        Bfx.Ctf30m
        [currencyPair|ADABTC|]
        Candles.optsDef
  case ecs of
    Left e ->
      error $ show e
    Right cs ->
      void
        . liftIO
        . GP.plotSync (SVG.cons "/app/build/output.svg")
        $ totalChart cs

totalChart ::
  NonEmpty Bfx.Candle ->
  Frame.T (Graph2D.T UTCTime Rational)
totalChart cs =
  Frame.cons
    ( Opts.key False $
        Opts.boxwidthRelative 1 Opts.deflt
    )
    $ candleChart cs
      <> mconcat
        ( mmaChart <$> Map.assocs (Mma.mmaCurves mma)
        )
      <> tradeChart
        ColorSpec.green
        ( Mma.unTradeEntry . fst <$> Mma.mmaTrades mma
        )
      <> tradeChart
        ColorSpec.red
        ( Mma.unTradeExit . snd <$> Mma.mmaTrades mma
        )
  where
    mma =
      Mma.mma cs

candleChart ::
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
candleChart =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth 0.99 $
          LineSpec.lineColor ColorSpec.beige LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . ((\x -> (Bfx.candleAt x, unQ $ Bfx.candleClose x)) <$>)
    . toList

mmaChart ::
  (Ma.MaPeriod, Map UTCTime Ma.Ma) ->
  Plot2D.T UTCTime Rational
mmaChart (_, xs) =
  ( Graph2D.lineSpec
      (LineSpec.lineWidth 0.7 LineSpec.deflt)
      <$>
  )
    . Plot2D.list Graph2D.lines
    . (second unMa <$>)
    $ Map.assocs xs

tradeChart ::
  ColorSpec.T ->
  [Candle] ->
  Plot2D.T UTCTime Rational
tradeChart color cs =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.5
        . LineSpec.pointType 2
        $ LineSpec.lineColor color LineSpec.deflt
    )
    <$> Plot2D.list
      Graph2D.points
      ((\x -> (Bfx.candleAt x, unQ $ Bfx.candleClose x)) <$> cs)

unMa :: Ma.Ma -> Rational
unMa =
  unQuotePerBase' . Ma.unMa

unQ :: Bfx.QuotePerBase 'Bfx.Buy -> Rational
unQ =
  from
