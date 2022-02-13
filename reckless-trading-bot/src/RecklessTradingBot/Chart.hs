{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Chart
  ( newExample,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Data.Candles as Candles
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import RecklessTradingBot.Import
import qualified Prelude

newExample :: (MonadIO m) => m ()
newExample = do
  ecs <-
    runExceptT
      . withExceptT ErrorBfx
      $ Bfx.candlesHist
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
    $ lineChart (Prelude.head cs) (Prelude.last cs)
      <> candleChart cs

lineChart ::
  Bfx.Candle ->
  Bfx.Candle ->
  Plot2D.T UTCTime Rational
lineChart x y =
  Plot2D.list
    Graph2D.linesPoints
    [ (Bfx.candleAt x, unQ $ Bfx.candleOpen x),
      (Bfx.candleAt y, unQ $ Bfx.candleClose y)
    ]

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

unQ :: Bfx.QuotePerBase 'Bfx.Buy -> Rational
unQ = from
