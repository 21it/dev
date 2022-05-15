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
        Bfx.Ctf6h
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
  [Bfx.Candle] ->
  Frame.T (Graph2D.T UTCTime Rational)
totalChart cs =
  Frame.cons
    ( Opts.key False $
        Opts.boxwidthRelative 1 $
          Opts.deflt
    )
    $ candleChart cs
      <> maChart cs

candleChart :: [Bfx.Candle] -> Plot2D.T UTCTime Rational
candleChart cs =
  Graph2D.lineSpec
    (LineSpec.lineColor ColorSpec.black LineSpec.deflt)
    <$> (Plot2D.list Graph2D.candleSticks $ candle2plot <$> cs)

candle2plot ::
  Bfx.Candle ->
  (UTCTime, (Rational, Rational, Rational, Rational))
candle2plot x =
  ( Bfx.candleAt x,
    ( unQ $ Bfx.candleOpen x,
      unQ $ Bfx.candleLow x,
      unQ $ Bfx.candleHigh x,
      unQ $ Bfx.candleClose x
    )
  )

maChart ::
  [Bfx.Candle] ->
  Plot2D.T UTCTime Rational
maChart =
  ( Graph2D.lineSpec
      (LineSpec.lineColor ColorSpec.cyan LineSpec.deflt)
      <$>
  )
    . Plot2D.list Graph2D.linesPoints
    . (second unMa <$>)
    . Map.assocs
    . Ma.ma20

unQ :: Bfx.QuotePerBase 'Bfx.Buy -> Rational
unQ =
  from

unMa :: Ma.Ma -> Rational
unMa =
  unQuotePerBase' . Ma.unMa
