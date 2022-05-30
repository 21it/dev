{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Chart
  ( newExample,
  )
where

import qualified BitfinexClient as Bfx
import BitfinexClient.Import
import qualified BitfinexClient.Indicator.Ma as Ma
import qualified BitfinexClient.Indicator.Mma as Mma
import qualified BitfinexClient.Trading as Trading
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Option
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG as SVG

newExample :: (MonadIO m) => m ()
newExample = do
  eMma <-
    runExceptT
      . Trading.theBestMma ctf
      $ CurrencyCode "BTC"
  case eMma of
    Left e ->
      error $ show e
    Right mma ->
      void
        . liftIO
        . GP.plotSync (SVG.cons "/app/build/output.svg")
        $ totalChart ctf mma
  where
    ctf = Ctf1m

totalChart ::
  CandleTimeFrame ->
  Mma.Mma ->
  Frame.T (Graph2D.T UTCTime Rational)
totalChart ctf mma =
  Frame.cons
    ( Opts.key True
        . Opts.title
          ( inspectStrPlain (Mma.mmaSymbol mma)
              <> " "
              <> T.unpack (toTextParam ctf)
              <> " candles, approx. profit rate = "
              <> ( T.unpack
                     . showPercent
                     . Mma.unApproxProfitRate
                     $ Mma.mmaProfit mma
                 )
          )
        . Opts.add
          ( Option.key "position"
          )
          [ pos,
            "reverse",
            "box",
            "opaque"
          ]
        $ Opts.boxwidthRelative 1 Opts.deflt
    )
    $ candleChart (Mma.mmaCandles mma)
      <> mconcat
        ( mmaChart <$> Map.assocs (Mma.mmaCurves mma)
        )
      <> tradeChart
        "Entry"
        ColorSpec.darkGreen
        ( Mma.unTradeEntry (Mma.mmaEntry mma) :
          (Mma.unTradeEntry . fst <$> Mma.mmaTrades mma)
        )
      <> tradeChart
        "Exit"
        ColorSpec.red
        ( Mma.unTradeExit . snd <$> Mma.mmaTrades mma
        )
  where
    cs = Mma.mmaCandles mma
    pos =
      if candleClose (head cs) > candleClose (last cs)
        then "right"
        else "left"

candleChart ::
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
candleChart =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth 1.5
          . LineSpec.title "Close"
          $ LineSpec.lineColor ColorSpec.gray80 LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . ((\x -> (Bfx.candleAt x, unQ $ Bfx.candleClose x)) <$>)
    . toList

mmaChart ::
  (Ma.MaPeriod, Map UTCTime Ma.Ma) ->
  Plot2D.T UTCTime Rational
mmaChart (period, xs) =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth 0.4
          . LineSpec.title
            ("MA " <> show (Ma.unMaPeriod period))
          $ LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . (second unMa <$>)
    $ Map.assocs xs

tradeChart ::
  String ->
  ColorSpec.T ->
  [Candle] ->
  Plot2D.T UTCTime Rational
tradeChart title color cs =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.24
        . LineSpec.pointType 7
        . LineSpec.title title
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
