{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module UnitNameTypeApps where

import TempPhantom (C, F, Temp (..), absoluteZero, paperBurning)

class UnitName u where
  unitName ∷ String

instance UnitName C where
  unitName ∷ String
  unitName = "C"

instance UnitName F where
  unitName ∷ String
  unitName = "F"

instance UnitName Temp where
  unitName ∷ String
  unitName = "_unspecified unit_"

instance UnitName u ⇒ UnitName (Temp u) where
  unitName ∷ UnitName u ⇒ String
  unitName = unitName @u

instance UnitName u ⇒ Show (Temp u) where
  show ∷ UnitName u ⇒ Temp u → String
  show (Temp t) = show t ++ "°" ++ unitName @u

unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName @u

unitAZ ∷ String
unitAZ = unit absoluteZero

unitPB ∷ String
unitPB = unit paperBurning

-- >>> unitAZ
-- "C"

-- >>> unitPB
-- "F"

