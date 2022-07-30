{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

import Fmt (Buildable (..), fmt, fmtLn, nameF, unwordsF, (+||), (||+))
import Radar (Direction (..), Turn (..), orientMany, rotateMany, rotateManySteps)
import System.Environment (getArgs, getProgName)

deriving instance Read Direction

deriving instance Read Turn

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "-"
  build TLeft = "←"
  build TRight = "→"
  build TAround = "¦"

rotateFromFile ∷ Direction → FilePath → IO ()
rotateFromFile dir fname = do
  f ← readFile fname
  let turns = map read $ lines f
      finalDir = rotateMany dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile ∷ FilePath → IO ()
orientFromFile fname = do
  f ← readFile fname
  let dirs = map read $ lines f
  fmt $ nameF "All turns" (unwordsF $ orientMany dirs)

{- ORMOLU_DISABLE -}
main ∷ IO ()
main = do
  args ← getArgs
  case args of
    ["-r", fname, dir] → rotateFromFile (read dir) fname
    ["-o", fname] → orientFromFile fname
    _ → do
      progName ← getProgName
      putStrLn $
        "Usage: " ++ progName ++ " -o filename\n" ++
        "       " ++ progName ++ " -r filename direction"
