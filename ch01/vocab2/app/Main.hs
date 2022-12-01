import Data.Char (isLetter)
import Data.Function ((&))
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)

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

printAllWords ∷ Vocabulary → IO ()
printAllWords vocab = do
  putStrLn "All words: "
  vocab & map fst & T.unlines & TIO.putStrLn

processTextFile ∷ FilePath → IO ()
processTextFile fname = do
  text ← TIO.readFile fname
  printAllWords $ extractVocab text

-- >>> text ← TIO.readFile "../../data/texts/hamlet.txt"
-- >>> take 10 $ extractVocab text
-- [("a",497),("a'th",2),("a-crosse",1),("a-downe",1),("a-downe-a",1),("a-dreames",1),("a-foot",1),("a-sleepe",1),("a-while",3),("a-worke",1)]

main ∷ IO ()
main = do
  args ← getArgs
  case args of
    [fname] → processTextFile fname
    _ → do
      progName ← getProgName
      putStrLn $ "Usage: " <> progName <> " `filename`"
