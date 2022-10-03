{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitNameProxies where

import Data.Proxy (Proxy (..))
import TempPhantom (C, F, K, Temp (..))

-- In Haskell, we don't have pattern matching over types.
-- The only way to distinguish different types in the same function is to define instances.
class UnitName u where
  unitName ∷ Proxy u → String

instance UnitName C where
  unitName ∷ Proxy C → String
  unitName _ = "C"

instance UnitName F where
  unitName ∷ Proxy F → String
  unitName _ = "F"

instance UnitName K where
  unitName ∷ Proxy K → String
  unitName _ = "K"

instance UnitName unit ⇒ UnitName (Temp unit) where
  unitName _ = unitName (Proxy ∷ Proxy unit)

instance UnitName Temp where
  unitName _ = "_unspecified unit_"

instance UnitName unit ⇒ Show (Temp unit) where
  show (Temp t) = show t ++ "°" ++ unitName (Proxy ∷ Proxy unit)

unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName (Proxy ∷ Proxy u)

someFunc ∷ proxy a → String
someFunc _ = "OK"
