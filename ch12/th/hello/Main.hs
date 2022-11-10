{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Hello (hello)

main ∷ IO ()
-- Splicing is like pasting source code.
-- hello ⇒ putStrLn "Hello world"
main = $hello -- Hello world
