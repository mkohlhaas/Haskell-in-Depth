{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Hello (hello)

main ∷ IO ()
main = $hello -- Hello world
