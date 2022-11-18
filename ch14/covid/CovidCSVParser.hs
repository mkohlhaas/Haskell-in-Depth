{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CovidCSVParser where

import Control.Applicative ((<|>))
import CovidData (CountryData (CountryData), CountryStat (CountryStat), DayCases (DayCases), DayDeaths (DayDeaths), DayInfo (DayInfo))
import Data.Attoparsec.ByteString.Char8 as A (Parser, char, count, decimal, double, endOfLine, option, parseOnly, skipWhile, take, takeTill, takeWhile)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (Day, fromGregorian)

-- Parsing steps:
-- 1. PARSE the original file to a sequence of pairs with a country code and the rest of a line.
-- 2. Group lines referring to one country together. (Done in the streaming part of the program.)
-- 3. PARSE the first line of a group.
-- 4. PARSE every other line by taking the interesting parts and skipping everything else.

------------------------
-- Parser combinators --
------------------------

data CountryCodeWithRest = CountryCodeWithRest
  { code ∷ !ByteString,
    rest ∷ !ByteString
  }
  deriving Show

countryCodeWithRest ∷ Parser CountryCodeWithRest
countryCodeWithRest = CountryCodeWithRest <$> countryCode <*> A.takeWhile notEOL <* endOfLine

-- >>> parseOnly countryCodeWithRest "DEU,Europe,Germany,2020-01-01,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,,,,,,,,,0.0,83783945.0,237.016,46.6,21.453,15.957,45229.245,,156.139,8.31,28.2,33.1,,8.0,81.33\n"
-- Right (CountryCodeWithRest {code = "DEU", rest = "Europe,Germany,2020-01-01,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,,,,,,,,,0.0,83783945.0,237.016,46.6,21.453,15.957,45229.245,,156.139,8.31,28.2,33.1,,8.0,81.33"})

countryCode ∷ Parser ByteString
countryCode = A.take 3 <* char ','

notEOL ∷ Char → Bool
notEOL c = c /= '\n' && c /= '\r'

countryCodeWithRestOrSkip ∷ Parser (Maybe CountryCodeWithRest)
countryCodeWithRestOrSkip = Just <$> countryCodeWithRest <|> Nothing <$ skipLine

-- some lines don't have a country code; see end of csv file
skipLine ∷ Parser ()
skipLine = skipWhile notEOL <* endOfLine

field ∷ Parser ByteString
field = takeTill (== ',') <* char ','

textField ∷ Parser Text
textField = decodeUtf8 <$> field

skipField ∷ Parser ()
skipField = skipWhile (/= ',') <* char ','

intField ∷ Parser Int
intField = (decimal <|> pure 0) <* skipField

-- >>> parseOnly intField "10,"
-- Right 10

-- >>> parseOnly intField "10.0,"
-- Right 10

-- >>> parseOnly intField ","
-- Right 0

-- >>> parseOnly intField ""
-- Left ",: not enough input"

dayInfoOnly ∷ Parser [(Day, DayInfo)]
dayInfoOnly = count 2 skipField *> dayInfo

dayInfo ∷ Parser [(Day, DayInfo)]
dayInfo = (\a b → [(a, b)]) <$> dayParser <*> dayInfoParser
  where
    dayParser = fromGregorian <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal <* char ','
    dayInfoParser = DayInfo <$> dayCasesParser <*> dayDeathsParser
    dayCasesParser = DayCases <$> intField <*> intField
    dayDeathsParser = DayDeaths <$> intField <*> intField

statInfo ∷ Parser CountryStat
--                        population        density
--                            |                |
statInfo = CountryStat <$> intField <*> option Nothing (Just <$> double)

fullCountryData ∷ ByteString → Parser CountryData
fullCountryData code =
  CountryData code
    <$> textField
    <*> textField
    <*> pure 0
    <*> pure 0
    <*> dayInfo
    <* count 14 skipField
    <*> statInfo

-- >>> parseOnly (fullCountryData "DEU") "Europe,Germany,2020-01-01,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,,,,,,,,,0.0,83783945.0,237.016,46.6,21.453,15.957,45229.245,,156.139,8.31,28.2,33.1,,8.0,81.33"
-- Right (CountryData {_iso_code = "DEU", _continent = "Europe", _name = "Germany", _current_total_cases = 0, _current_total_deaths = 0, _days = [(2020-01-01,DayInfo {_cases = DayCases {_total_cases = 0, _new_cases = 0}, _deaths = DayDeaths {_total_deaths = 0, _new_deaths = 0}})], _stat = CountryStat {_population = 83783945, _population_density = Just 237.016}})

-- Parsers for the pipeline stages in Main

parseFullCountryData ∷ CountryCodeWithRest → Maybe CountryData
parseFullCountryData CountryCodeWithRest {..} = either (const Nothing) Just $ parseOnly (fullCountryData code) rest

parseDayInfo ∷ CountryCodeWithRest → [(Day, DayInfo)]
parseDayInfo CountryCodeWithRest {..} = fromRight [] $ parseOnly dayInfoOnly rest
