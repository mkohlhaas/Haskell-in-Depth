import Data.Char (isLetter, toLower)
import Data.Function ((&))
import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

-- >>> text ← readFile "../../data/texts/hamlet.txt"
-- >>> take 7 $ map head $ group $ sort $ words $ map toLower text
-- ["&","'em?","'gainst","'tane","'tis","'tis,","'twas"]

-- >>> :t group
-- group ∷ Eq a ⇒ [a] → [[a]]

-- >>> text <- readFile "../../data/texts/hamlet.txt"
-- >>> ws = words $ map toLower text
-- >>> ws' = map (takeWhile isLetter . dropWhile (not . isLetter)) ws
-- >>> cleanedWords = filter (not . null) ws'
-- >>> uniqueWords = map head $ group $ sort cleanedWords
-- >>> take 7 uniqueWords
-- >>> length uniqueWords
-- ["a","abhominably","abhorred","abilitie","aboord","aboue","about"]
-- 4633

-- >>> text ← TIO.readFile  "../../data/texts/hamlet.txt"
-- >>> text & T.words & map (T.dropAround $ not . isLetter) & filter (not . T.null) & map T.toCaseFold & sort & group & map head & take 5
-- ["a","a'th","a-crosse","a-downe","a-downe-a"]

main ∷ IO ()
main = do
  [fname] ← getArgs
  text ← TIO.readFile fname
  let ws = text & T.words & map (T.dropAround $ not . isLetter) & filter (not . T.null) & map T.toCaseFold & sort & group & map head
  TIO.putStrLn $ T.unwords ws
  TIO.putStrLn $ T.unlines ws
  print $ length ws
