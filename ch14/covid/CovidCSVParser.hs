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

data CountryCodeWithRest = CountryCodeWithRest
  { code :: ByteString,
    rest :: ByteString
  }

-- Parser combinators

notEOL :: Char -> Bool
notEOL c = c /= '\n' && c /= '\r'

countryCode :: Parser ByteString
countryCode = A.take 3 <* char ','

countryCodeWithRest :: Parser CountryCodeWithRest
countryCodeWithRest =
  CountryCodeWithRest <$> countryCode
    <*> A.takeWhile notEOL
    <* endOfLine

skipLine :: Parser ()
skipLine = skipWhile notEOL <* endOfLine

countryCodeWithRestOrSkip :: Parser (Maybe CountryCodeWithRest)
countryCodeWithRestOrSkip =
  Just <$> countryCodeWithRest <|> Nothing <$ skipLine

field :: Parser ByteString
field = takeTill (== ',') <* char ','

textField :: Parser Text
textField = decodeUtf8 <$> field

skipField :: Parser ()
skipField = skipWhile (/= ',') <* char ','

intField :: Parser Int
intField = (decimal <|> pure 0) <* skipField

fullCountryData :: ByteString -> Parser CountryData
fullCountryData code =
  CountryData code
    <$> textField
    <*> textField
    <*> pure 0
    <*> pure 0
    <*> dayInfo
    <* count 14 skipField
    <*> statInfo

dayInfoOnly :: Parser [(Day, DayInfo)]
dayInfoOnly = count 2 skipField *> dayInfo

dayInfo :: Parser [(Day, DayInfo)]
dayInfo = (\a b -> [(a, b)]) <$> dayParser <*> dayInfoParser
  where
    dayParser = fromGregorian <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal <* char ','
    dayInfoParser = DayInfo <$> dayCasesParser <*> dayDeathsParser
    dayCasesParser = DayCases <$> intField <*> intField
    dayDeathsParser = DayDeaths <$> intField <*> intField

statInfo :: Parser CountryStat
statInfo =
  CountryStat <$> intField -- population
    <*> option Nothing (Just <$> double) -- density

-- Parsers

parseFullCountryData :: CountryCodeWithRest -> Maybe CountryData
parseFullCountryData CountryCodeWithRest {..} =
  either (const Nothing) Just $ parseOnly (fullCountryData code) rest

parseDayInfo :: CountryCodeWithRest -> [(Day, DayInfo)]
parseDayInfo CountryCodeWithRest {..} =
  fromRight [] $ parseOnly dayInfoOnly rest
