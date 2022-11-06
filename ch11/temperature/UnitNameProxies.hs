{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitNameProxies where

import Data.Proxy (Proxy (..))
import TempPhantom (C, F, K, Temp (..), absoluteZero, paperBurning)

-- In Haskell we don't have pattern matching over types.
-- Not allowed. Type Error: Couldn't match expected type ‘Double’ with actual type ‘C’/'F'/'K'
-- instance UnitName unit ⇒ Show (Temp unit) where
--   show (Temp (t ∷ C)) = show t ++ "°C"
--   show (Temp (t ∷ F)) = show t ++ "°F"
--   show (Temp (t ∷ K)) = show t ++ "°K"

-- The only way to distinguish different types in the same function is to define instances.

-- # Show Temp
instance UnitName unit ⇒ Show (Temp unit) where
  show ∷ UnitName unit ⇒ Temp unit → String
  show (Temp t) = show t ++ "°" ++ unitName (Proxy ∷ Proxy unit)

-- # UnitName
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
  unitName ∷ UnitName unit ⇒ Proxy (Temp unit) → String
  unitName _ = unitName (Proxy ∷ Proxy unit)

-- >>> :kind Temp
-- Temp ∷ Type → Type

-- PolyKinds makes this possible
instance UnitName Temp where
  unitName ∷ Proxy Temp → String
  unitName _ = "_unspecified unit_"

-- get unit from temperature value
unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName (Proxy ∷ Proxy u)

-- >>> unitName (Proxy ∷ Proxy F)
-- "F"

-- >>> unitName (Proxy ∷ Proxy C)
-- "C"

-- >>> unitName (345 ∷ Temp C)
-- Couldn't match expected type ‘Proxy u0’ with actual type ‘Temp C’

-- >>> :type (345 ∷ Temp C)
-- (345 ∷ Temp C) ∷ Temp C

unitPB ∷ String
unitPB = unit paperBurning

unitAZ ∷ String
unitAZ = unit absoluteZero

unitNN ∷ String
unitNN = unitName (Proxy ∷ Proxy Temp)

-- >>> unitPB
-- "F"

-- >>> unitAZ
-- "C"

-- >>> unitNN
-- "_unspecified unit_"

-- `proxy` is a type variable(!)
someFunc ∷ proxy a → String
someFunc _ = "OK"

-- We can supply any argument with a type that is a type constructor with a type argument, for example:

-- Maybe a
-- >>> someFunc (Just 'x')
-- "OK"

-- Maybe a
-- >>> someFunc (Right 'x')
-- "OK"

-- [a]
-- >>> someFunc []
-- "OK"

-- Proxy a
-- >>> someFunc (Proxy ∷ Proxy Bool)
-- "OK"

-- [Char]
-- >>> someFunc "hello"
-- "OK"

-- Either Bool a
-- >>> someFunc (Left False)
-- "OK"

-- no type param
-- >>> someFunc True
-- Couldn't match expected type ‘proxy0 a0’ with actual type ‘Bool’

-- no type param
-- >>> someFunc 'M'
-- Couldn't match expected type ‘proxy0 a0’ with actual type ‘Char’

-- no type param
-- >>> someFunc (42 ∷ Int)
-- Couldn't match expected type ‘proxy0 a0’ with actual type ‘Int’

-- Although this technique is also used, the specific Proxy or Tagged type gives much more control over kinds and should be preferred nowadays.
