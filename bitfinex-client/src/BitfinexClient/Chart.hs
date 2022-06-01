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
      . Trading.theBestMma ctf [moneyQuoteBuy|10|]
      $ CurrencyCode "BTC"
  case eMma of
    Left e -> do
      putStrLn (show e :: Text)
      liftIO $ threadDelay 30000000
      newExample
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
              <> " candles, approx. reward/risk = "
              <> ( T.unpack
                     . showPercent
                     . Mma.unRewardToRisk
                     $ Mma.mmaRewardToRisk mma
                 )
          )
        . Opts.add
          ( Option.key "position"
          )
          [ "left",
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
      <> takeProfitChart
        ( Mma.mmaEntry mma : (fst <$> Mma.mmaTrades mma)
        )
      <> tradeChart
        "Exit"
        ColorSpec.red
        ( Mma.unTradeExit . snd <$> Mma.mmaTrades mma
        )

candleChart ::
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
candleChart =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth 1.5
          . LineSpec.title "High"
          $ LineSpec.lineColor ColorSpec.gray80 LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . ((\x -> (Bfx.candleAt x, unQ $ Bfx.candleHigh x)) <$>)
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

takeProfitChart ::
  [Mma.TradeEntry] ->
  Plot2D.T UTCTime Rational
takeProfitChart xs =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.24
        . LineSpec.title "Entry"
        $ LineSpec.lineColor ColorSpec.darkMagenta LineSpec.deflt
    )
    <$> Plot2D.list
      Graph2D.financeBars
      ( ( \x ->
            ( Bfx.candleAt $ Mma.tradeEntryCandle x,
              ( unQ . Bfx.candleClose $ Mma.tradeEntryCandle x,
                unQ' . Mma.unStopLoss $ Mma.tradeEntryStopLoss x,
                unQ' . Mma.unTakeProfit $ Mma.tradeEntryTakeProfit x,
                unQ . Bfx.candleClose $ Mma.tradeEntryCandle x
              )
            )
        )
          <$> xs
      )

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

unQ' :: Bfx.QuotePerBase' -> Rational
unQ' =
  unQuotePerBase'
