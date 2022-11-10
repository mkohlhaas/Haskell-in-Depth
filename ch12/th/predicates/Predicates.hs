{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- 1. Reify the given data-type name, and extract information about data constructors.
-- 2. Write code to generate trivial predicate definitions with TH.
-- 3. Use TH splices to actually generate code for predicates.

-- The reify function is extremely powerful: it opens the door to the compilation pipeline and reports almost
-- every piece of information known to the compiler at the moment when TH is fired up.

module Predicates (mkPredicates) where

import Language.Haskell.TH

-- 1. Reify the given data-type name, and extract information about data constructors.
mkPredicates ∷ Name → Q [Dec]
mkPredicates name = reify name >>= fmap concat . mapM mkPredicate . extractConstructors

-- >>> :type reify
-- reify ∷ Name → Q Info

-- 2. Write code to generate trivial predicate definitions with TH.
extractConstructors ∷ Info → [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors _ = []

mkPredicate ∷ Con → Q [Dec]
mkPredicate (NormalC name types) =
  [d|
    $predicate = \case
      $pat → True
      _ → False
    |]
  where
    predicate ∷ PatQ
    predicate = varP $ mkName $ "is" ++ nameBase name
    pat ∷ PatQ
    pat = conP name $ replicate (length types) wildP
mkPredicate _ = pure []
