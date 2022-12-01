{-# LANGUAGE InstanceSigs #-}
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
import Fmt (Buildable (..), Builder, blockListF, fmt, hexF, nameF, unlinesF, (+|), (|+))
import GHC.Generics
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

data Entry = Entry
  { word ∷ !Text,
    freq ∷ !Int
  }
  deriving (Show)

type Vocabulary = [Entry]

instance Buildable Entry where
  build ∷ Entry → Builder
  build (Entry word freq) = "" +| word |+ ": " +| freq |+ ""

--------------------
-- Pure Functions --
--------------------

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
    buildEntry xs@(x : _) = Entry x (length xs)

allWords ∷ Vocabulary → [Text]
allWords = map word

type TotalWords = Int
type UniqueWords = Int

wordsCount ∷ Vocabulary → (TotalWords, UniqueWords)
wordsCount vocab = (sum $ map freq vocab, length vocab)

wordsByFrequency ∷ Vocabulary → Vocabulary
wordsByFrequency = sortOn (Down . freq)

-------------
-- Reports --
-------------

allWordsReport ∷ Vocabulary → Text
allWordsReport vocab = fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport ∷ Vocabulary → Text
wordsCountReport vocab = fmt $ "Total number of words: " +| totalWords |+ "\nNumber of unique words: " +| uniqueWords |+ "\n"
  where
    (totalWords, uniqueWords) = wordsCount vocab

frequentWordsReport ∷ Vocabulary → Int → Text
-- frequentWordsReport vocab num = fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
frequentWordsReport vocab num = fmt $ nameF "Frequent words" $ blockListF reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (word, freq) = "" +| word |+ ": " +| freq |+ ""

--------
-- IO --
--------

processTextFile ∷ FilePath → Bool → Int → IO ()
processTextFile fname withAllWords n = do
  text ← TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

-- >>> text ← TIO.readFile "../../data/texts/hamlet.txt"
-- >>> vocab = extractVocab text
-- >>> take 5 vocab
-- >>> wordsCountReport vocab
-- >>> frequentWordsReport vocab 10
-- [Entry {word = "a", freq = 497},Entry {word = "a'th", freq = 2},Entry {word = "a-crosse", freq = 1},Entry {word = "a-downe", freq = 1},Entry {word = "a-downe-a", freq = 1}]
-- "Total number of words: 29575\nNumber of unique words: 4827\n"
-- "Frequent words:\n  - the: 993\n  - and: 862\n  - to: 683\n  - of: 610\n  - i: 547\n  - you: 522\n  - my: 502\n  - a: 497\n  - it: 415\n  - in: 384\n"

-- >>> name = "John"
-- >>> age = 30
-- >>> fmt $ "Hello, " +| name |+ "!\nI know that your age is " +|age|+ ".\n" ∷ String
-- >>> fmt $ "That is age " +| hexF age |+ " in hex!\n" ∷ String
-- "Hello, John!\nI know that your age is 30.\n"
-- "That is age 1e in hex!\n"

-- >>> fmt $ nameF "clients" $ blockListF ["Alice", "Bob", "Zalgo"] ∷ String
-- "clients:\n  - Alice\n  - Bob\n  - Zalgo\n"

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
