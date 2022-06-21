{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module SuffixedStrings (SuffixedString, suffixed, asString) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- Example: suffixed strings

newtype SuffixedString (suffix :: Symbol) = SuffixedString String

suffixed :: String -> SuffixedString suffix
suffixed = SuffixedString

asString :: ∀ suffix. KnownSymbol suffix => SuffixedString suffix -> String
asString (SuffixedString str) = str ++ "@" ++ symbolVal (Proxy :: Proxy suffix)
