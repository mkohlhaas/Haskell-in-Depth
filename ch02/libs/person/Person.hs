{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Person where

import Data.String (IsString (..))

type Age = Int

type Name = String

data Person = Person !Name !(Maybe Age)

homer ∷ Person
homer = Person "Homer Simpson" (Just 39)

-- >>> :info IsString
-- type IsString ∷ * → Constraint
-- class IsString a where
--   fromString ∷ String → a
--   {-# MINIMAL fromString #-}

instance IsString Person where
  fromString ∷ String → Person
  fromString name = Person name Nothing

spj ∷ Person
spj = "Simon Peyton Jones"
