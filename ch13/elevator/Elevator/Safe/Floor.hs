{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Elevator.Safe.Floor where

import Data.Type.Dec
import Data.Type.Equality
import Data.Type.Nat
import Data.Type.Nat.LE

-- definition of Nat
-- data Nat = Z | S Nat

type GoodFloor max cur = (SNatI max, SNatI cur, LE cur max)

-- GoodFloor is a Constraint/triple of Constraints.
-- >>> :kind GoodFloor
-- GoodFloor ∷ Nat > Nat → Constraint

-- So is SNatI.
-- >>> :kind SNatI
-- SNatI ∷ Nat → Constraint

-- So is LE.
-- >>> :kind LE
-- LE ∷ Nat → Nat → Constraint

-- SNatI max: we can always get a natural number out of the natural number.
-- LE cur max: `cur` is less or equal to `max`.

data Floor (max ∷ Nat) (cur ∷ Nat) where
  MkFloor ∷ GoodFloor max cur ⇒ Floor max cur

instance Show (Floor max cur) where
  show ∷ Floor max cur → String
  show MkFloor =
    "Floor " <> show (snatToNat (snat ∷ SNat cur))
      <> " of "
      <> show (snatToNat (snat ∷ SNat max))

-- >>> MkFloor ∷ Floor Nat5 Nat0
-- Floor 0 of 5

type BelowTop max cur = LE (S cur) max

-- another Constraint
-- >>> :kind BelowTop
-- BelowTop ∷ Nat → Nat → Constraint

next ∷ BelowTop max cur ⇒ Floor max cur → Floor max (S cur)
next MkFloor = MkFloor

prev ∷ ∀ max cur. Floor max (S cur) → Floor max cur
prev MkFloor =
  withSNat snatCur $
    withLEProof
      leCur
      MkFloor
  where
    snatCur ∷ SNat cur
    snatCur = case snat ∷ SNat (S cur) of
      SS → snat
    leCur ∷ LEProof cur max
    leCur = leStepL leProof

sameFloor ∷ ∀ max to from. Floor max to → Floor max from → Maybe (to :~: from)
sameFloor MkFloor MkFloor = eqNat

-- >>> sameFloor (MkFloor ∷ Floor Nat5 Nat3) (MkFloor ∷ Floor Nat5 Nat3)
-- Just Refl

-- >>> sameFloor (MkFloor ∷ Floor Nat5 Nat3) (MkFloor ∷ Floor Nat5 Nat0)
-- Nothing

mkFloor ∷ ∀ max cur. (SNatI max, SNatI cur) ⇒ Maybe (Floor max cur)
mkFloor =
  case decideLE ∷ Dec (LEProof cur max) of
    Yes prf → withLEProof prf $ Just MkFloor
    No _ → Nothing
