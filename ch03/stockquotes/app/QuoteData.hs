{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QuoteData where

import Data.ByteString.Char8 (unpack)
import Data.Csv (FromField (..), FromNamedRecord)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Fmt (Buildable (..), Builder, fixedF, pretty, (+|), (+||), (|+), (||+))
import GHC.Generics (Generic)

data QuoteData = QuoteData
  { day ∷ Day,
    volume ∷ Int,
    open ∷ Double,
    close ∷ Double,
    high ∷ Double,
    low ∷ Double
  }
  deriving (Generic, FromNamedRecord, Show)

instance Buildable QuoteData where
  build QuoteData {..} = "Day: " +| day |+ " Volume: " +| volume |+ " Open: " +| open |+ " Close: " +| close |+ " High: " +| high |+ ""

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

data QField = Open | Close | High | Low | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

field2fun ∷ QField → QuoteData → Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume
