{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type LineNumber = Int

data ErrorMsg = WrongFormat LineNumber Text
  deriving (Show)

type SQL = Writer [ErrorMsg] Text

genInsert ∷ Text → Text → Text
genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processLine ∷ (LineNumber, Text) → SQL
processLine (_, T.splitOn ":" → [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genSQL ∷ Text → SQL
genSQL txt = txt & T.lines & zip [1 ..] & traverse processLine <&> T.concat

testData ∷ Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL ∷ IO ()
testGenSQL = do
  let (sql, errors) = runWriter $ genSQL testData
  TIO.putStrLn "Generated SQL:"
  TIO.putStrLn sql
  TIO.putStrLn "Errors:"
  traverse_ print errors

main ∷ IO ()
main = testGenSQL

-- Generated SQL:
-- INSERT INTO items VALUES ('Pen','Bob');
-- INSERT INTO items VALUES ('Pencil','Alice');
-- INSERT INTO items VALUES ('Book','Bob');
--
-- Errors:
-- WrongFormat 2 "Glass:Mary:10"
-- WrongFormat 5 "Bottle"
