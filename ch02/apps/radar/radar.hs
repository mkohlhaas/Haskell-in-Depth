{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Fmt (Buildable (..), Builder, fmt, fmtLn, nameF, unwordsF, (+||), (||+))
import Radar (Direction (..), Turn (..), orientMany, rotateMany, rotateManySteps)
import System.Environment (getArgs, getProgName)

instance Buildable Direction where
  build ∷ Direction → Builder
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build ∷ Turn → Builder
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
  fmtLn $ "Final direction: " +|| finalDir ||+ "" -------- using Show instance of Direction (for 'finalDir') from a use of ‘||+’
  fmt $ nameF "Intermediate directions" (unwordsF dirs) -- using Buildable instance of Direction (for 'dirs')

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
