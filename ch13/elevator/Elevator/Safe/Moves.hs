{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Elevator.Safe.Moves where

import Data.Type.Dec
import Data.Type.Equality
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Void
import Elevator.Safe.Floor

data Move mx to from where
  StandStill ∷ Move mx to to
  GoingUp ∷ BelowTop mx from ⇒ Move mx to from
  GoingDown ∷ from ~ S fl ⇒ Move mx to from

decideMove ∷
  ∀ mx to from.
  Floor mx to →
  Floor mx from →
  Move mx to from
decideMove MkFloor MkFloor =
  case discreteNat ∷ Dec (to :~: from) of
    Yes Refl → StandStill -- to == from
    No to_neq_from →
      case decideLE ∷ Dec (LEProof to from) of
        Yes to_le_from →
          -- to < from
          withAboveGround to_le_from to_neq_from GoingDown
        No to_gt_from →
          -- to > from
          withLEProof (belowTop to_gt_from) GoingUp
  where
    belowTop ∷ LE to mx ⇒ Neg (LEProof to from) → LEProof (S from) mx
    belowTop neg = leTrans (leSwap neg) leProof

    withAboveGround ∷
      LEProof to from →
      Neg (to :~: from) →
      (∀ fl. from ~ S fl ⇒ r) →
      r
    withAboveGround (LESucc _) _ r = r
    withAboveGround LEZero neq r =
      case snat ∷ SNat from of
        SZ → absurd $ neq Refl
        SS → r
