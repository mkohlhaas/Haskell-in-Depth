{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Str

verse ∷ String
verse =
  [str|What needs my Shakespeare for his honoured bones,
The labor of an age in pilèd stones,
Or that his hallowed relics should be hid
Under a star-ypointing pyramid?
Dear son of Memory, great heir of fame,
What need’st thou such weak witness of thy name?
Thou in our wonder and astonishment
Hast built thyself a live-long monument…|]

-- >>> verse
-- "What needs my Shakespeare for his honoured bones,\nThe labor of an age in pil\232d stones,\nOr that his hallowed relics should be hid\nUnder a star-ypointing pyramid?\nDear son of Memory, great heir of fame,\nWhat need\8217st thou such weak witness of thy name?\nThou in our wonder and astonishment\nHast built thyself a live-long monument\8230"

main ∷ IO ()
main = putStrLn verse
