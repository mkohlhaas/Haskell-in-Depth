{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- aligned pointers
module Pointers (Pointer, ptrValue, inc, maybePtr, zeroPtr) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, symbolVal)

-- GHC.TypeLits module promotes Nat to a kind and natural numbers to types.
-- `align` is a phantom type
newtype Pointer (align ∷ Nat) = Pointer Integer
  deriving (Show)

--------------------------------------------
-- Side Note for KnownNat and KnownSymbol --
--------------------------------------------

-- class KnownNat (n ∷ Nat)
-- This class gives the integer associated with a type-level natural.
-- There are instances of the class for every concrete literal: 0, 1, 2, etc.
-- natVal ∷ ∀ n proxy. KnownNat n ⇒ proxy n → Integer

-- >>> natVal (Proxy ∷ Proxy 5)
-- 5

-- class KnownSymbol (n ∷ Symbol)
-- This class gives the string associated with a type-level symbol.
-- There are instances of the class for every concrete literal: "hello", etc.
-- symbolVal ∷ ∀ n proxy. KnownSymbol n ⇒ proxy n → String

-- >>> symbolVal (Proxy ∷ Proxy "hello")
-- "hello"

-- KnownNat because it must be known at compile time.

----------------------
-- End of Side Note --
----------------------

ptrValue ∷ ∀ align. KnownNat align ⇒ Pointer align → Integer
ptrValue (Pointer p) = p * natVal (Proxy ∷ Proxy align)

-- >>> show (Pointer 42 ∷ Pointer 4)
-- "Pointer 42"
--
-- >>> ptrValue (Pointer 42 ∷ Pointer 4)
-- 168

inc ∷ Pointer align → Pointer align
inc (Pointer p) = Pointer (p + 1)

-- >>> inc (Pointer 42 ∷ Pointer 4)
-- Pointer 43
--
-- >>> ptrValue $ inc (Pointer 42 ∷ Pointer 4)
-- 172

maybePtr ∷ ∀ align. KnownNat align ⇒ Integer → Maybe (Pointer align)
maybePtr p
  | reminder == 0 = Just $ Pointer quotient
  | otherwise = Nothing
  where
    (quotient, reminder) = divMod p (natVal (Proxy ∷ Proxy align))

-- >>> maybePtr 4 ∷ Maybe (Pointer 8)
-- Nothing
--
-- >>> ptrValue <$> (maybePtr 4 ∷ Maybe (Pointer 8))
-- Nothing
--
-- >>> maybePtr 8 ∷ Maybe (Pointer 4)
-- Just (Pointer 2)
--
-- >>> ptrValue <$> (maybePtr 8 ∷ Maybe (Pointer 4))
-- Just 8

-- zero pointer is pointer of any alignment at 0
zeroPtr ∷ Pointer n
zeroPtr = Pointer 0

-- >>> ptrValue (zeroPtr ∷ Pointer 8)
-- 0
--
-- >>> ptrValue (zeroPtr ∷ Pointer 4)
-- 0
--
-- >>> inc (inc $ zeroPtr ∷ Pointer 4)
-- Pointer 2
--
-- >>> ptrValue (inc (inc $ zeroPtr ∷ Pointer 4))
-- 8

