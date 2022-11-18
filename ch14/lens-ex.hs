{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Control.Lens
import Data.Monoid (Endo (Endo, appEndo))

data Point = Point {_x ∷ Double, _y ∷ Double}
  deriving (Show, Eq)

data Segment = Segment {_beg ∷ Point, _end ∷ Point}
  deriving (Show)

makeLenses ''Point
makeLenses ''Segment

segs ∷ [Segment]
segs =
  [ Segment (Point 0 0) (Point 100 0),
    Segment (Point 100 0) (Point 0 100),
    Segment (Point 0 100) (Point 0 0)
  ]

-- >>> toListOf (folded . beg) segs
-- >>> segs ^.. folded . beg
-- >>> segs & traverse . end . y .~ 0
-- >>> set (traverse . end . y) 0 segs
-- >>> over (traverse . beg . x) (+100) segs
-- [Point {_x = 0.0, _y = 0.0},Point {_x = 100.0, _y = 0.0},Point {_x = 0.0, _y = 100.0}]
-- [Point {_x = 0.0, _y = 0.0},Point {_x = 100.0, _y = 0.0},Point {_x = 0.0, _y = 100.0}]
-- [Segment {_beg = Point {_x = 0.0, _y = 0.0}, _end = Point {_x = 100.0, _y = 0.0}},Segment {_beg = Point {_x = 100.0, _y = 0.0}, _end = Point {_x = 0.0, _y = 0.0}},Segment {_beg = Point {_x = 0.0, _y = 100.0}, _end = Point {_x = 0.0, _y = 0.0}}]
-- [Segment {_beg = Point {_x = 0.0, _y = 0.0}, _end = Point {_x = 100.0, _y = 0.0}},Segment {_beg = Point {_x = 100.0, _y = 0.0}, _end = Point {_x = 0.0, _y = 0.0}},Segment {_beg = Point {_x = 0.0, _y = 100.0}, _end = Point {_x = 0.0, _y = 0.0}}]
-- [Segment {_beg = Point {_x = 100.0, _y = 0.0}, _end = Point {_x = 100.0, _y = 0.0}},Segment {_beg = Point {_x = 200.0, _y = 0.0}, _end = Point {_x = 0.0, _y = 100.0}},Segment {_beg = Point {_x = 100.0, _y = 100.0}, _end = Point {_x = 0.0, _y = 0.0}}]

move ∷ Double → [Segment] → [Segment]
move d =
  over (traverse . beg . x) (+ d)
    . over (traverse . beg . y) (+ d)
    . over (traverse . end . x) (+ d)
    . over (traverse . end . y) (+ d)

-- Lenses are first-class values, so we can put them into a list.
move' ∷ Double → [Segment] → [Segment]
move' d = compose [over (traverse . point . coord) (+ d) | point ← [beg, end], coord ← [x, y]]
  where
    compose ∷ [[Segment] → [Segment]] → [Segment] → [Segment]
    compose = appEndo . foldMap Endo

-- >>> :type foldMap
-- foldMap ∷ (Foldable t, Monoid m) ⇒ (a → m) → t a → m

-- The `Endo a` newtype is a wrapper over `a → a` functions (they are called endomorphisms).
-- >>> :type Endo
-- Endo ∷ (a → a) → Endo a

-- We get the wrapped function via the appEndo function.
-- >>> :type appEndo
-- appEndo ∷ Endo a → a → a

-- The Monoid instance for Endo is implemented to build a composition of two such Endo functions!

-- >>> pair = ("hello, world", '!')
-- >>> view _1 pair
-- >>> view _2 pair
-- >>> pair ^. _1
-- >>> pair ^. _2
-- >>> set _1 "bye" pair
-- >>> pair & _1 .~ "bye"
-- >>> _1 .~ "bye" $ pair
-- >>> over _1 length pair
-- >>> _1 %~ length $ pair
-- >>> pair & _1 %~ length
-- >>> view (_2._1) (False, pair)
-- >>> (False, pair) ^. _2 . _1
-- "hello, world"
-- '!'
-- "hello, world"
-- '!'
-- ("bye",'!')
-- ("bye",'!')
-- ("bye",'!')
-- (12,'!')
-- (12,'!')
-- (12,'!')
-- "hello, world"
-- "hello, world"

-- >>> points = ("points", [(1, 1), (0, 1), (1, 0)]) ∷ (String, [(Int, Int)])
-- >>> view _2 points
-- >>> toListOf (_2 . folded . _1) points
-- >>> points ^.. _2 . folded. _1
-- >>> toListOf (_2 . ix 1) points
-- >>> points ^.. _2 . ix 1
-- >>> set (_2 . traverse . _1) 'x' points
-- >>> points & _2 . traverse . _1 .~ 'x'
-- >>> over (_2 . traverse . _1) (*100) points
-- >>> points & _2 . traverse . _1 %~ (*100)
-- [(1,1),(0,1),(1,0)]
-- [1,0,1]
-- [1,0,1]
-- [(0,1)]
-- [(0,1)]
-- ("points",[('x',1),('x',1),('x',0)])
-- ("points",[('x',1),('x',1),('x',0)])
-- ("points",[(100,1),(0,1),(100,0)])
-- ("points",[(100,1),(0,1),(100,0)])

-- >>> info = ("info", Right 5) ∷ (String, Either String Int)
-- >>> view _2 info
-- >>> preview (_2 . _Right) info
-- >>> preview (_2 . _Left) info
-- >>> info ^? (_2 . _Right)
-- >>> info & _2 . _Right .~ (0 ∷ Int)
-- >>> info & _2 . _Right %~ (+1)
-- Right 5
-- Just 5
-- Nothing
-- Just 5
-- ("info",Right 0)
-- ("info",Right 6)

main ∷ IO ()
main = print $ move' 100 segs
