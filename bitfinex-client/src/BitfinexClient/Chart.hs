{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Chart
  ( newExample,
    newMmaHeader,
  )
where

import qualified BitfinexClient as Bfx
import BitfinexClient.Import
import qualified BitfinexClient.Indicator.Ma as Ma
import BitfinexClient.Indicator.Mma (Mma)
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

newtype MmaHeader = MmaHeader
  { unMmaHeader :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      NFData
    )
  deriving stock
    ( Generic
    )

newExample :: (MonadIO m) => m ()
newExample = do
  eMma <-
    runExceptT
      . Trading.theBestMma ctf [moneyQuoteBuy|30000|]
      $ CurrencyCode "USD"
  case eMma of
    Left e -> do
      putStrLn (show e :: Text)
      putStrLn ("Sleeping 30 seconds..." :: Text)
      liftIO $ threadDelay 30000000
      putStrLn ("Trying again..." :: Text)
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
          ( T.unpack . unMmaHeader $
              newMmaHeader ctf mma
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
    $ candleChart start (Mma.mmaCandles mma)
      <> mconcat
        ( uncurry (maChart start)
            <$> Map.assocs (Mma.mmaCurves mma)
        )
      <> entryChart
        ( Mma.mmaEntry mma : (fst <$> Mma.mmaTrades mma)
        )
      <> exitChart
        ( Mma.mmaTrades mma
        )
  where
    start = Mma.mmaDataFrom mma

newMmaHeader :: CandleTimeFrame -> Mma -> MmaHeader
newMmaHeader ctf mma =
  MmaHeader $
    inspectPlain (Mma.mmaSymbol mma)
      <> ", trade entry = "
      <> displaySats
        ( from $
            candleClose entryCandle
        )
      <> ", take profit = "
      <> displaySats
        ( unQ' . Mma.unTakeProfit $
            Mma.tradeEntryTakeProfit tradeEntry
        )
      <> ", stop loss = "
      <> displaySats
        ( unQ' . Mma.unStopLoss $
            Mma.tradeEntryStopLoss tradeEntry
        )
      <> ",\n"
      <> "profit = "
      <> showPercent
        ( unProfitRate $
            Mma.tradeEntryProfitRate tradeEntry
        )
      <> ", risk/reward = "
      <> inspectPlain (denominator r2r)
      <> "/"
      <> inspectPlain (numerator r2r)
      <> ", candle timeframe = "
      <> toTextParam ctf
      <> ", time = "
      <> formatAsLogTime (candleAt entryCandle)
      <> " UTC"
  where
    r2r = Mma.unRewardToRisk $ Mma.mmaRewardToRisk mma
    tradeEntry = Mma.mmaEntry mma
    entryCandle = Mma.tradeEntryCandle tradeEntry

candleChart ::
  UTCTime ->
  NonEmpty Bfx.Candle ->
  Plot2D.T UTCTime Rational
candleChart start =
  ( Graph2D.lineSpec
      ( LineSpec.lineWidth lineSize
          . LineSpec.title "OHLC"
          $ LineSpec.lineColor ColorSpec.gray80 LineSpec.deflt
      )
      <$>
  )
    . Plot2D.list Graph2D.financeBars
    . ( ( \x ->
            ( Bfx.candleAt x,
              ( unQ $ Bfx.candleOpen x,
                unQ $ Bfx.candleLow x,
                unQ $ Bfx.candleHigh x,
                unQ $ Bfx.candleClose x
              )
            )
        )
          <$>
      )
    . filter ((>= start) . Bfx.candleAt)
    . toList

maChart ::
  UTCTime ->
  Ma.MaPeriod ->
  Map UTCTime Ma.Ma ->
  Plot2D.T UTCTime Rational
maChart start period xs =
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
    . filter ((>= start) . fst)
    $ Map.assocs xs

entryChart ::
  [Mma.TradeEntry] ->
  Plot2D.T UTCTime Rational
entryChart xs =
  Graph2D.lineSpec
    ( LineSpec.lineWidth entrySize
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

exitChart ::
  [(Mma.TradeEntry, Mma.TradeExit)] ->
  Plot2D.T UTCTime Rational
exitChart cs =
  Graph2D.lineSpec
    ( LineSpec.pointSize 0.2
        . LineSpec.pointType 9
        . LineSpec.title "Exit"
        $ LineSpec.lineColor ColorSpec.darkMagenta LineSpec.deflt
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
