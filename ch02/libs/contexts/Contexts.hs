{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Contexts where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Writer (MonadWriter (tell, writer), Writer, runWriter)
import Control.Monad.Writer.Lazy (runWriterT)
import Text.Read (readMaybe)

readNumber ∷ IO Int
readNumber = do
  s ← getLine
  pure $ read s

-- the same:
-- readNumber ∷ IO Int
-- readNumber = read <$> getLine

sumN ∷ Int → Writer [String] Int
sumN 0 = writer (0, ["finish"])
sumN n = do
  tell [show n]
  s ← sumN $ n -1
  pure $ n + s

-- >>> sumN 10
-- WriterT (Identity (55,["10","9","8","7","6","5","4","3","2","1","finish"]))
--
-- >>> runIdentity $ runWriterT $ sumN 10
-- (55,["10","9","8","7","6","5","4","3","2","1","finish"])
--
-- >>> runWriter $ sumN 10
-- (55,["10","9","8","7","6","5","4","3","2","1","finish"])
--
-- >>> :t traverse sumN
-- traverse sumN :: Traversable t => t Int -> WriterT [String] Identity (t Int)
--
-- >>> traverse sumN [4..5]
-- WriterT (Identity ([10,15],["4","3","2","1","finish","5","4","3","2","1","finish"]))
--
-- >>> sequence $ traverse sumN [4..5]
-- [WriterT (Identity (10,["4","3","2","1","finish","5","4","3","2","1","finish"])),WriterT (Identity (15,["4","3","2","1","finish","5","4","3","2","1","finish"]))]
--
-- >>> traverse readMaybe ["1", "2", "3"] ∷ Maybe [Int]
-- Just [1,2,3]
--
-- >>> traverse readMaybe ["1", "two", "3"] ∷ Maybe [Int]
-- Nothing
--
-- >>> sequence Nothing
-- Nothing

cartesianProduct ∷ [Int] → [Int] → [(Int, Int)]
cartesianProduct xs ys = do
  x ← xs
  y ← ys
  pure (x, y)

-- >>> cartesianProduct [1..2] [1..3]
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]

addNumber ∷ Int → IO String
addNumber n = (<>) (show n <> " ") <$> getLine
