{-# OPTIONS_GHC -Wno-deprecations #-}
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
import qualified Control.Parallel.Strategies as Par
import qualified Data.Map as Map
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
    runExceptT $ do
      syms <-
        Bfx.symbolsDetails
      cs <-
        mapM
          ( \sym ->
              (sym,)
                <$> Bfx.candlesHist
                  Bfx.Ctf1m
                  sym
                  Candles.optsDef
                    { Candles.limit = Just 10000
                    }
          )
          . traceShowId
          . take 15
          . filter ((== CurrencyCode "BTC") . currencyPairQuote)
          $ Map.keys syms
      case nonEmpty cs of
        Nothing ->
          error "Can not find BTC-quoted symbols"
        Just ncs ->
          pure
            . maximum
            . Par.withStrategy (Par.parTraversable Par.rdeepseq)
            $ uncurry Mma.mma <$> ncs
  case eMma of
    Left e ->
      error $ show e
    Right mma ->
      void
        . liftIO
        . GP.plotSync (SVG.cons "/app/build/output.svg")
        $ totalChart mma

totalChart ::
  Mma.Mma ->
  Frame.T (Graph2D.T UTCTime Rational)
totalChart mma =
  Frame.cons
    ( Opts.key True
        . Opts.add (Option.key "position") [pos, "reverse"]
        $ Opts.boxwidthRelative 1 Opts.deflt
    )
    $ candleChart (Mma.mmaCandles mma)
      <> mconcat
        ( mmaChart <$> Map.assocs (Mma.mmaCurves mma)
        )
      <> tradeChart
        "Entry"
        ColorSpec.darkGreen
        ( Mma.unTradeEntry . fst <$> Mma.mmaTrades mma
        )
      <> tradeChart
        "Exit"
        ColorSpec.red
        ( Mma.unTradeExit . snd <$> Mma.mmaTrades mma
        )
      <> maybe mempty entryChart (Mma.mmaEntry mma)
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

entryChart ::
  Mma.TradeEntry ->
  Plot2D.T UTCTime Rational
entryChart (Mma.TradeEntry x) =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.5
        . LineSpec.pointType 13
        . LineSpec.title "Now"
        $ LineSpec.lineColor ColorSpec.goldenrod LineSpec.deflt
    )
    <$> Plot2D.list
      Graph2D.points
      [(Bfx.candleAt x, unQ $ Bfx.candleClose x)]

unMa :: Ma.Ma -> Rational
unMa =
  unQuotePerBase' . Ma.unMa

unQ :: Bfx.QuotePerBase 'Bfx.Buy -> Rational
unQ =
  from
