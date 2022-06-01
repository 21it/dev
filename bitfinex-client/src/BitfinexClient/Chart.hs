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
            "opaque",
            "box"
          ]
        $ Opts.boxwidthRelative 1 Opts.deflt
    )
    $ candleChart (Mma.mmaCandles mma)
      <> mconcat
        ( maChart <$> Map.assocs (Mma.mmaCurves mma)
        )
      <> entryChart
        ( Mma.mmaEntry mma : (fst <$> Mma.mmaTrades mma)
        )
      <> exitChart
        ( Mma.mmaTrades mma
        )

candleChart ::
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
candleChart =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth lineSize
          . LineSpec.title "High"
          $ LineSpec.lineColor ColorSpec.gray80 LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . ((\x -> (Bfx.candleAt x, unQ $ Bfx.candleHigh x)) <$>)
    . toList

maChart ::
  (Ma.MaPeriod, Map UTCTime Ma.Ma) ->
  Plot2D.T UTCTime Rational
maChart (period, xs) =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth lineSize
          . LineSpec.title
            ("MA " <> show (Ma.unMaPeriod period))
          $ LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.lines
    . (second unMa <$>)
    $ Map.assocs xs

entryChart ::
  [Mma.TradeEntry] ->
  Plot2D.T UTCTime Rational
entryChart xs =
  Graph2D.lineSpec
    ( LineSpec.lineWidth entrySize
        . LineSpec.title "Entry"
        $ LineSpec.lineColor ColorSpec.black LineSpec.deflt
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

exitChart ::
  [(Mma.TradeEntry, Mma.TradeExit)] ->
  Plot2D.T UTCTime Rational
exitChart cs =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.2
        . LineSpec.pointType 9
        . LineSpec.title "Exit"
        $ LineSpec.lineColor ColorSpec.navy LineSpec.deflt
    )
    <$> Plot2D.list
      Graph2D.points
      ( ( \(entry, exit) ->
            ( Bfx.candleAt $
                Mma.unTradeExit exit,
              unQ' . Mma.unTakeProfit $
                Mma.tradeEntryTakeProfit entry
            )
        )
          <$> cs
      )

unMa :: Ma.Ma -> Rational
unMa =
  unQuotePerBase' . Ma.unMa

unQ :: Bfx.QuotePerBase 'Bfx.Buy -> Rational
unQ =
  from

unQ' :: Bfx.QuotePerBase' -> Rational
unQ' =
  unQuotePerBase'

lineSize :: Double
lineSize =
  0.5

entrySize :: Double
entrySize =
  0.6
