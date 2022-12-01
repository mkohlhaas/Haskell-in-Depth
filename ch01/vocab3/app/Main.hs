{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Char (isLetter)
import Data.Function ((&))
import Data.List (group, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt (blockListF', fmt, nameF, unlinesF, (+|), (|+))
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocab ∷ Text → Vocabulary
extractVocab text =
  text
    & T.words
    & map cleanWord
    & filter (not . T.null)
    & map T.toCaseFold
    & sort
    & group
    & map buildEntry
  where
    cleanWord = T.dropAround (not . isLetter)
    buildEntry [] = error "unexpected"
    buildEntry xs@(x : _) = (x, length xs)

allWords ∷ Vocabulary → [Text]
allWords = map fst

wordsCount ∷ Vocabulary → (Int, Int)
wordsCount vocab = (sum $ map snd vocab, length vocab)

wordsByFrequency ∷ Vocabulary → Vocabulary
wordsByFrequency = sortOn (Down . snd)

allWordsReport ∷ Vocabulary → Text
allWordsReport vocab = fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport ∷ Vocabulary → Text
wordsCountReport vocab = fmt $ "Total number of words: " +| totalWords |+ "\nNumber of unique words: " +| uniqueWords |+ "\n"
  where
    (totalWords, uniqueWords) = wordsCount vocab

frequentWordsReport ∷ Vocabulary → Int → Text
frequentWordsReport vocab num = fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (word, count) = "" +| word |+ ": " +| count |+ ""

processTextFile ∷ FilePath → Bool → Int → IO ()
processTextFile fname withAllWords n = do
  text ← TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main ∷ IO ()
main = do
  args ← getArgs
  case args of
    ["-a", fname, num] → processTextFile fname True (read num)
    [fname, num] → processTextFile fname False (read num)
    _ → do
      progName ← getProgName
      putStrLn $ "Usage: " ++ progName ++ " [-a] `filename` `number of words`"
  where
    read num = fromMaybe 10 $ readMaybe num
