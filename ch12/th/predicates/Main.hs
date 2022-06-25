{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Predicates (mkPredicates)

data Shape
  = Circle Double
  | Square Double
  | Triangle Double Double Double

$(mkPredicates ''Shape)

main :: IO ()
main = mapM_ print [isCircle s1, isSquare s2, isTriangle s3, isCircle s2, isSquare s3, isTriangle s1]
  where
    s1 = Circle 4
    s2 = Square 10
    s3 = Triangle 1 1 1
