{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module SuffixedStrings (SuffixedString, suffixed, asString) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- GHC.TypeLits module promotes String to the kind Symbol, strings such as "hello" and "bye" to types.

-- suffixed strings
newtype SuffixedString (suffix ∷ Symbol) = SuffixedString String

suffixed ∷ String → SuffixedString suffix
suffixed = SuffixedString

asString ∷ ∀ suffix. KnownSymbol suffix ⇒ SuffixedString suffix → String
asString (SuffixedString str) = str ++ "@" ++ symbolVal (Proxy ∷ Proxy suffix)

-- >>> asString (suffixed "bravit" ∷ SuffixedString "teachers")
-- "bravit@teachers"

-- >>> asString (suffixed "bravit" ∷ SuffixedString "devs")
-- "bravit@devs"
