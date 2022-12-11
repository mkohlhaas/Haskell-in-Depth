{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module SuffixedStrings (SuffixedString, suffixed) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- GHC.TypeLits module promotes String to the kind Symbol, strings such as "hello" and "bye" to types.

-- suffixed strings
newtype SuffixedString (suffix ∷ Symbol) = SuffixedString String

suffixed ∷ String → SuffixedString suffix
suffixed = SuffixedString

instance KnownSymbol suffix ⇒ Show (SuffixedString suffix) where
  show ∷ KnownSymbol suffix ⇒ SuffixedString suffix → String
  show (SuffixedString str) = str ++ "@" ++ symbolVal (Proxy ∷ Proxy suffix)

-- >>> suffixed "bravit" ∷ SuffixedString "teachers"
-- bravit@teachers
--
-- >>> suffixed "bravit" ∷ SuffixedString "devs"
-- bravit@devs

