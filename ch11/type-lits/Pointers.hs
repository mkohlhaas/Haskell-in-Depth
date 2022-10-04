{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- aligned pointers
module Pointers (Pointer, ptrValue, inc, maybePtr, zeroPtr) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)

-- GHC.TypeLits module promotes Nat to a kind and natural numbers to types.
newtype Pointer (align ∷ Nat) = Pointer Integer
  deriving (Show)

-- The KnownNat type derives from the Nat type its values (going from type to value)
-- The KnownNat type class defines the natVal method, which takes a type-level natural literal to its term-level integer counterpart.
-- KnownNat because it must be known at compile time.
ptrValue ∷ ∀ align. KnownNat align ⇒ Pointer align → Integer
ptrValue (Pointer p) = p * natVal (Proxy ∷ Proxy align)

-- |
-- >>> inc (inc $ zeroPtr ∷ Pointer 4)
-- Pointer 2

-- |
-- >>> ptrValue (inc (inc $ zeroPtr ∷ Pointer 4))
-- 8

inc ∷ Pointer align → Pointer align
inc (Pointer p) = Pointer (p + 1)

maybePtr ∷ ∀ align. KnownNat align ⇒ Integer → Maybe (Pointer align)
maybePtr p
  | reminder == 0 = Just $ Pointer quotient
  | otherwise = Nothing
  where
    (quotient, reminder) = divMod p (natVal (Proxy ∷ Proxy align))

-- |
-- >>> maybePtr 4 ∷ Maybe (Pointer 8)
-- Nothing

-- |
-- >>> ptrValue <$> (maybePtr 4 ∷ Maybe (Pointer 8))
-- Nothing

-- |
-- >>> maybePtr 8 ∷ Maybe (Pointer 4)
-- Just (Pointer 2)

-- |
-- >>> ptrValue <$> (maybePtr 8 ∷ Maybe (Pointer 4))
-- Just 8

-- zero pointer is pointer of any alignment at 0
zeroPtr ∷ Pointer n
zeroPtr = Pointer 0

-- |
-- >>> ptrValue (zeroPtr ∷ Pointer 8)
-- 0

-- |
-- >>> ptrValue (zeroPtr ∷ Pointer 4)
-- 0
