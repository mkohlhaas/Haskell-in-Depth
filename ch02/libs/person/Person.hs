{-# LANGUAGE OverloadedStrings #-}

module Person where

import Data.String

type Age = Int

type Name = String

data Person = Person Name (Maybe Age)

instance IsString Person where
  fromString name = Person name Nothing

homer :: Person
homer = Person "Homer Simpson" (Just 39)

spj :: Person
spj = "Simon Peyton Jones"
