{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Charts (plotChart)
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import HtmlReport (htmlReport)
import Params (Params (..), cmdLineParser)
import QuoteData (QuoteData)
import StatReport (statInfo, textReport)

generateReports ∷ (Functor t, Foldable t) ⇒ Params → t QuoteData → IO ()
generateReports Params {..} quotes = do
  unless silent $ putStr textRpt
  when chart $ plotChart title quotes chartFname
  saveHtml htmlFile htmlRpt
  where
    statInfo' = statInfo quotes
    textRpt = textReport statInfo'
    htmlRpt = htmlReport title quotes statInfo' [chartFname | chart]

    withCompany prefix = maybe mempty (prefix <>) company
    chartFname = unpack $ "chart" <> withCompany "_" <> ".svg"
    title = unpack $ "Historical Quotes" <> withCompany " for "

    saveHtml Nothing _ = pure ()
    saveHtml (Just f) html = BL.writeFile f html

-- TODO: Params probably should be ReaderT
work ∷ Params → IO ()
work params = do
  csvData ← BL.readFile $ fname params
  case decodeByName csvData of
    Left err → putStrLn err
    Right (_, quotes) → generateReports params quotes

-- >>> take 1 <$> readQuotes "../../data/quotes.csv"
-- [QuoteData {day = 2019-05-01, volume = 64827300, open = 209.880005, close = 210.520004, high = 215.309998, low = 209.229996}]

readQuotes ∷ FilePath → IO [QuoteData]
readQuotes fpath = do
  csvData ← BL.readFile fpath
  case decodeByName csvData of
    Left err → error err
    Right (_, quotes) → pure $ toList quotes

main ∷ IO ()
main = cmdLineParser >>= work
