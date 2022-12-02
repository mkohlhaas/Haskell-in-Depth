{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Radar where

import Data.Monoid

class (Eq a, Enum a, Bounded a) ⇒ CyclicEnum a where
  cpred ∷ a → a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc ∷ a → a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West
  deriving (Eq, Enum, Bounded, CyclicEnum, Read, Show)

-- >>> csucc North
-- East

-- >>> csucc West
-- North

-- >>> minBound ∷ Direction
-- North

-- >>> maxBound ∷ Direction
-- West

-- >>> show West
-- "West"

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Enum, Bounded, Read, Show)

-- >>> succ TNone
-- TLeft

-- >>> succ TAround
-- succ{Turn}: tried to take `succ' of last tag in enumeration

instance Semigroup Turn where
  (<>) ∷ Turn → Turn → Turn
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

-- >>> Sum 2 <> Sum 3
-- Sum {getSum = 5}

-- >>> getSum $ Sum 2 <> Sum 3
-- 5

-- >>> Product 2 <> Product 3
-- Product {getProduct = 6}

-- >>> TAround <> TAround
-- TNone

-- >>> TAround <> TRight
-- TLeft

-- >>> h = "hello"
-- >>> c = ", "
-- >>> w = "world"
-- >>> h <> c <> w
-- "hello, world"

instance Monoid Turn where
  mempty ∷ Turn
  mempty = TNone

-- >>> mempty ∷ [Int]
-- []

-- >>> mempty ∷ String
-- ""

-- >>> mempty ∷ Sum Int
-- Sum {getSum = 0}

-- >>> mempty ∷ Product Int
-- Product {getProduct = 1}

-- >>> mconcat [TLeft, TRight, TAround, TAround]
-- TNone

rotate ∷ Turn → Direction → Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

-- >>> rotate TLeft East
-- North

-- >>> rotate TRight North
-- East

-- >>> rotate (read "TAround") (read "North")
-- South

-- >>> f ← readFile "turns.txt"
-- >>> lines f
-- >>> map read $ lines f ∷ [Turn]
-- ["TAround","TNone","TLeft","TRight","TRight","TNone","TAround","TLeft","TLeft","TRight","TRight","TNone","TNone","TLeft","TRight","TNone","TAround"]
-- [TAround,TNone,TLeft,TRight,TRight,TNone,TAround,TLeft,TLeft,TRight,TRight,TNone,TNone,TLeft,TRight,TNone,TAround]

rotateMany ∷ Direction → [Turn] → Direction
rotateMany = foldl (flip rotate)

-- >>> rotateMany North [TLeft, TAround, TRight]
-- South

rotateMany' ∷ Direction → [Turn] → Direction
rotateMany' dir ts = rotate (mconcat ts) dir

-- >>> rotateMany' North [TLeft, TAround, TRight]
-- South

rotateManySteps ∷ Direction → [Turn] → [Direction]
rotateManySteps = scanl (flip rotate)

-- >>> rotateManySteps North [TLeft, TAround, TRight]
-- [North,West,East,South]

every ∷ (Enum a, Bounded a) ⇒ [a]
every = enumFrom minBound

-- >>> every ∷ [Turn]
-- [TNone,TLeft,TRight,TAround]

-- >>> every ∷ [Direction]
-- [North,East,South,West]

orient ∷ Direction → Direction → Turn
orient d1 d2 = head $ filter (\t → rotate t d1 == d2) every

-- >>> orient North South
-- TAround

-- >>> orient North West
-- TLeft

orientMany ∷ [Direction] → [Turn]
orientMany ds = zipWith orient ds (tail ds)

-- >>> orientMany [North, West, East, South]
-- [TLeft,TAround,TRight]

-- >>> orientMany []
-- []

-- >>> orientMany [West]
-- []

-- >>> orientMany [North, West]
-- [TLeft]

-- >>> orientMany [North, West, East, North]
-- [TLeft,TAround,TLeft]

