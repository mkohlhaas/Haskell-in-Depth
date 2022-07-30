{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Person where

import Data.String (IsString (..))

type Age = Int

type Name = String

data Person = Person Name (Maybe Age)

-- https://hackage.haskell.org/package/base-4.16.3.0/docs/Data-String.html#t:IsString
-- Used by the overloaded string extension (-XOverloadedStrings in GHC).
instance IsString Person where
  fromString name = Person name Nothing

homer ∷ Person
homer = Person "Homer Simpson" (Just 39)

spj ∷ Person
spj = "Simon Peyton Jones"
