import Data.Char (isLetter)
import Data.Function ((&))
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab text = ws & sort & group & map buildEntry
  where
    ws = text & T.words & map cleanWord & filter (not . T.null) & map T.toCaseFold
    buildEntry [] = error "unexpected"
    buildEntry xs@(x : _) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  vocab & map fst & T.unlines & TIO.putStrLn

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processTextFile fname
    _ -> putStrLn "Usage: vocab2 filename"
