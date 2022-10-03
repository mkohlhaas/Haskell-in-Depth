{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UnitNameTypeApps where

import TempPhantom (C, F, Temp (..))

class UnitName u where
  unitName ∷ String

instance UnitName C where
  unitName = "C"

instance UnitName F where
  unitName = "F"

instance UnitName Temp where
  unitName = "_unspecified unit_"

instance UnitName u ⇒ UnitName (Temp u) where
  unitName = unitName @u

instance UnitName u ⇒ Show (Temp u) where
  show (Temp t) = show t ++ "°" ++ unitName @u

unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName @u
