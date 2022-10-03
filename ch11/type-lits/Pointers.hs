{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Example: aligned pointers
module Pointers (Pointer, ptrValue, inc, maybePtr, zeroPtr) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)

newtype Pointer (align ∷ Nat) = Pointer Integer

ptrValue ∷ ∀ align. KnownNat align ⇒ Pointer align → Integer
ptrValue (Pointer p) = p * natVal (Proxy ∷ Proxy align)

inc ∷ Pointer align → Pointer align
inc (Pointer p) = Pointer $ p + 1

maybePtr ∷ ∀ align. KnownNat align ⇒ Integer → Maybe (Pointer align)
maybePtr p
  | reminder == 0 = Just $ Pointer quotient
  | otherwise = Nothing
  where
    (quotient, reminder) = divMod p (natVal (Proxy ∷ Proxy align))

zeroPtr ∷ Pointer n
zeroPtr = Pointer 0
