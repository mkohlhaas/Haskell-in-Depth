{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Hello

main :: IO ()
main = $hello
