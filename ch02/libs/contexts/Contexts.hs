{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Contexts where

import Control.Monad.Writer (MonadWriter (tell, writer), Writer, runWriter)
import Control.Monad.Writer.Lazy (runWriterT)

readNumber ∷ IO Int
readNumber = do
  s ← getLine
  pure $ read s

-- the same:
-- readNumber ∷ IO Int
-- readNumber = read <$> getLine

-- >>> sumN 10
-- WriterT (Identity (55,["10","9","8","7","6","5","4","3","2","1","finish"]))

-- >>> runWriterT $ sumN 10
-- Identity (55,["10","9","8","7","6","5","4","3","2","1","finish"])

-- >>> runWriter $ sumN 10
-- (55,["10","9","8","7","6","5","4","3","2","1","finish"])

sumN ∷ Int → Writer [String] Int
sumN 0 = writer (0, ["finish"])
sumN n = do
  tell [show n]
  s ← sumN $ n -1
  pure $ n + s

-- >>> cartesianProduct [1..2] [1..3]
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]

cartesianProduct ∷ [Int] → [Int] → [(Int, Int)]
cartesianProduct xs ys = do
  x ← xs
  y ← ys
  pure (x, y)

addNumber ∷ Int → IO String
addNumber n = pure (++) <*> pure (show n ++ " ") <*> getLine

-- the same:
-- addNumber ∷ Int → IO String
-- addNumber n = (++) (show n ++ " ") <$> getLine
