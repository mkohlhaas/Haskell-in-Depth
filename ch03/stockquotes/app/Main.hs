{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Charts (plotChart)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import HtmlReport (htmlReport, colStats)
import Params (Params (..), cmdLineParser)
import QuoteData (QuoteData)
import StatReport (statInfo, textReport)
import Debug.Trace (trace, traceShow)

generateReports ∷ (Functor t, Foldable t) ⇒ Params → t QuoteData → IO ()
generateReports params@Params {..} quotes = do
  let fu = traceShow "fname" (show params)
  putStrLn fu
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

-- guards test ANY Haskell expression
-- >>> ["Fu" | True ]
-- >>> ["Fu" | False ]
-- ["Fu"]
-- []

    withCompany prefix = maybe mempty (prefix <>) company
    chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html

readQuotes ∷ FilePath → IO [QuoteData]
readQuotes fpath = do
  csvData ← BL.readFile fpath
  case decodeByName csvData of
    Left err → error err
    Right (_, quotes) → pure $ toList quotes

-- >>> quotes ← take 3 <$> readQuotes "../../data/quotes.csv"
-- >>> quotes
-- >>> statInfo quotes
-- [QuoteData {day = 2019-05-01, volume = 64827300, open = 209.880005, close = 210.520004, high = 215.309998, low = 209.229996},
--  QuoteData {day = 2019-05-02, volume = 31996300, open = 209.839996, close = 209.149994, high = 212.649994, low = 208.130005},
--  QuoteData {day = 2019-05-03, volume = 20892400, open = 210.889999, close = 211.75, high = 211.839996, low = 210.229996}]
-- [StatEntry {qfield = Open, meanVal = StatValue {decimalPlaces = 2, value = 210.20333333333335}, minVal = StatValue {decimalPlaces = 2, value = 209.839996}, maxVal = StatValue {decimalPlaces = 2, value = 210.889999}, daysBetweenMinMax = 1},
--  StatEntry {qfield = Close, meanVal = StatValue {decimalPlaces = 2, value = 210.47333266666666}, minVal = StatValue {decimalPlaces = 2, value = 209.149994}, maxVal = StatValue {decimalPlaces = 2, value = 211.75}, daysBetweenMinMax = 1},
--  StatEntry {qfield = High, meanVal = StatValue {decimalPlaces = 2, value = 213.26666266666666}, minVal = StatValue {decimalPlaces = 2, value = 211.839996}, maxVal = StatValue {decimalPlaces = 2, value = 215.309998}, daysBetweenMinMax = 2},
--  StatEntry {qfield = Low, meanVal = StatValue {decimalPlaces = 2, value = 209.1966656666667}, minVal = StatValue {decimalPlaces = 2, value = 208.130005}, maxVal = StatValue {decimalPlaces = 2, value = 210.229996}, daysBetweenMinMax = 1},
--  StatEntry {qfield = Volume, meanVal = StatValue {decimalPlaces = 2, value = 3.9238666666666664e7}, minVal = StatValue {decimalPlaces = 0, value = 2.08924e7}, maxVal = StatValue {decimalPlaces = 0, value = 6.48273e7}, daysBetweenMinMax = 2}]

-- >>> import Fmt
-- >>> quotes ← readQuotes "../../data/quotes.csv"
-- >>> si = statInfo quotes
-- >>> unlinesF si
-- "Open: 202.04 (mean), 175.44 (min), 224.80 (max), 100 (days)
--  Close: 202.16 (mean), 173.30 (min), 223.59 (max), 100 (days)
--  High: 204.10 (mean), 177.92 (min), 226.42 (max), 101 (days)
--  Low: 200.32 (mean), 170.27 (min), 222.86 (max), 101 (days)
--  Volume: 27869192.38 (mean), 11362000 (min), 69281400 (max), 28 (days)
-- "

-- >>> quotes ← readQuotes "../../data/quotes.csv"
-- >>> textReport $ statInfo quotes
-- "+-------------+-------------+----------+----------+-------------------+
--  | Quote Field | Mean        | Min      | Max      | Days btw. Min/Max |
--  +-------------+-------------+----------+----------+-------------------+
--  | Open        | 202.04      | 175.44   | 224.80   | 100               |
--  | Close       | 202.16      | 173.30   | 223.59   | 100               |
--  | High        | 204.10      | 177.92   | 226.42   | 101               |
--  | Low         | 200.32      | 170.27   | 222.86   | 101               |
--  | Volume      | 27869192.38 | 11362000 | 69281400 | 28                |
--  +-------------+-------------+----------+----------+-------------------+
-- "

main ∷ IO ()
main = cmdLineParser >>= work
  where
    work ∷ Params → IO ()
    work params = do
      csvData ← BL.readFile $ fname params
      case decodeByName csvData of
        Left err → putStrLn err
        Right (_, quotes) → generateReports params quotes
