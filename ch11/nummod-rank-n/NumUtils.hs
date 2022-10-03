{-# LANGUAGE RankNTypes #-}

module NumUtils where

newtype NumModifier = NumModifier {run ∷ ∀ a. Num a ⇒ a → a}
