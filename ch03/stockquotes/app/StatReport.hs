{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module StatReport where

import Colonnade (Colonnade, Headed, ascii, headed)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Text hiding (length)
import Data.Time (diffDays)
import Fmt (Buildable (..), Builder, fixedF, pretty, (+|), (+||), (|+), (||+))
import QuoteData (QField (Volume), QuoteData (day), field2fun)

decimalPlacesFloating ∷ Int
decimalPlacesFloating = 2

data StatValue = StatValue
  { decimalPlaces ∷ !Int,
    value ∷ !Double
  }
  deriving (Show)

-- statistics for one field (Open, Close, ...)
data StatEntry = StatEntry
  { qfield ∷ !QField,
    meanVal ∷ !StatValue,
    minVal ∷ !StatValue,
    maxVal ∷ !StatValue,
    daysBetweenMinMax ∷ !Int
  }
  deriving (Show)

mean ∷ (Fractional a, Foldable t) ⇒ t a → a
mean xs = sum xs / fromIntegral (length xs)

minMaxDays ∷ (Ord a, Foldable t) ⇒ (QuoteData → a) → t QuoteData → (a, a, Int)
minMaxDays get quotes = (minVal, maxVal, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp quotes
    maxQ = maximumBy cmp quotes
    minVal = get minQ
    maxVal = get maxQ
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo ∷ (Foldable t, Functor t) ⇒ t QuoteData → [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound]
  where
    decimalPlacesByQField Volume = 0
    decimalPlacesByQField _ = decimalPlacesFloating

    qFieldStatInfo qfield =
      let get = field2fun qfield
          (mn, mx, daysBetweenMinMax) = minMaxDays get quotes
          decPlaces = decimalPlacesByQField qfield
          meanVal = StatValue decimalPlacesFloating (mean $ fmap get quotes)
          minVal = StatValue decPlaces mn
          maxVal = StatValue decPlaces mx
       in StatEntry {..}

instance Buildable StatValue where
  build ∷ StatValue → Builder
  build sv = fixedF (decimalPlaces sv) (value sv)

-- for use in the REPL
instance Buildable StatEntry where
  build ∷ StatEntry → Builder
  build StatEntry {..} =
    qfield ||+ ": "
      +| meanVal |+ " (mean), "
      +| minVal |+ " (min), "
      +| maxVal |+ " (max), "
      +| daysBetweenMinMax |+ " (days)"

colStats ∷ Colonnade Headed StatEntry String
colStats =
  mconcat
    [ headed "Quote Field" (show . qfield),
      headed "Mean" (pretty . meanVal),
      headed "Min" (pretty . minVal),
      headed "Max" (pretty . maxVal),
      headed "Days btw. Min/Max" (pretty . daysBetweenMinMax)
    ]

textReport ∷ [StatEntry] → String
textReport = ascii colStats

showPrice ∷ Double → Builder
showPrice = fixedF decimalPlacesFloating
