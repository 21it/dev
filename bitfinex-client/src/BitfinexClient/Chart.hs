{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Chart
  ( newExample,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.Candles as Candles
import BitfinexClient.Import
import qualified BitfinexClient.Indicator.Ma as Ma
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
        Opts.boxwidthRelative 1 $
          Opts.deflt
    )
    $ maChart ColorSpec.seaGreen 200 cs
      <> maChart ColorSpec.darkRed 50 cs
      <> maChart ColorSpec.black 20 cs

maChart ::
  ColorSpec.T ->
  Ma.MaPeriod ->
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
maChart color maPeriod =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth 0.7 $
          LineSpec.lineColor color LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . (second unMa <$>)
    . Map.assocs
    . Ma.ma maPeriod

unMa :: Ma.Ma -> Rational
unMa =
  unQuotePerBase' . Ma.unMa
