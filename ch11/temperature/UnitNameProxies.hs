{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitNameProxies where

import Data.Proxy (Proxy (..))
import TempPhantom (C, F, K, Temp (..))

-- In Haskell we don't have pattern matching over types.
-- Not allowed. Type Error: Couldn't match expected type ‘Double’ with actual type ‘C’/'F'/'K'
-- instance UnitName unit ⇒ Show (Temp unit) where
--   show (Temp (t ∷ C)) = show t ++ "°C"
--   show (Temp (t ∷ F)) = show t ++ "°F"
--   show (Temp (t ∷ K)) = show t ++ "°K"
-- The only way to distinguish different types in the same function is to define instances.
instance UnitName unit ⇒ Show (Temp unit) where
  show (Temp t) = show t ++ "°" ++ unitName (Proxy ∷ Proxy unit)

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

-- get unit from temperature value
unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName (Proxy ∷ Proxy u)

-- `proxy` is a type variable
someFunc ∷ proxy a → String
someFunc _ = "OK"

-- We can supply any argument with a type that is a type constructor with a type argument, for example:
-- ghci> someFunc (Just 'x') → "OK"
-- ghci> someFunc (Right 'x') → "OK"
-- ghci> someFunc [] → "OK"
-- ghci> someFunc (Proxy :: Proxy Bool) → "OK"
-- Although this technique is also used, the specific Proxy or Tagged type gives much more control over kinds and should be preferred nowadays.
