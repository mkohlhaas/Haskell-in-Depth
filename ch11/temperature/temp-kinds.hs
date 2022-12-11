{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoStarIsType #-}

-- Defining and using new kinds besides Type and Constraint.

-- With DataKinds enabled, this line defines both the TempUnits data type with values F and C and the TempUnits kind with types F and C belonging to it.
-- value → type → kind (values have a type, types have kinds)
-- Promoting types to kinds and values to types!
-- Type constructors become kind constructors.
-- Data constructors become type constructors.
-- Now we have not only Type and Constraint kinds but also TempUnits.
-- We say that F and C values are promoted to types.
-- Consequently, the TempUnits type is promoted to kind.
data TempUnits = F | C

-- `F` is a value and a type.
-- TempUnits is a type and a kind.

-- >>> :type F
-- F ∷ TempUnits

-- >>> :kind F
-- F ∷ TempUnits

-- >>> :kind TempUnits
-- TempUnits ∷ Type

-- Sometimes we or the compiler need to disambiguate equally named values and corresponding promoted types.
-- In these situations, a tick (') is used to mention a type and not a value.

-- `u` is a phantom type but only of a certain kind(!), i.e. TempUnits with the only possible types(!) `F` and `C`
newtype Temp (u ∷ TempUnits) = Temp Double
  deriving (Num, Fractional)

paperBurning ∷ Temp F
paperBurning = 451

absoluteZero ∷ Temp C
absoluteZero = -273.15

-- >>> paperBurning - paperBurning
-- 0.0°F
--
-- >>> absoluteZero - absoluteZero
-- 0.0°C
--
-- >>> paperBurning - absoluteZero
-- Couldn't match type ‘'C’ with ‘'F’
-- Expected type: Temp 'F
--   Actual type: Temp 'C

f2c ∷ Temp F → Temp C
f2c (Temp f) = Temp ((f -32) * 5 / 9)

-- Type Error: Expected kind ‘TempUnits’, but ‘Bool’ has kind ‘*’
-- nonsense ∷ Temp Bool
-- nonsense = Temp 0

-- Type Error: Couldn't match type ‘C’ with ‘F’ ...
-- err = paperBurning - absoluteZero

diff ∷ Temp C
diff = f2c paperBurning - absoluteZero

-- We can also mention our new kind in type class declarations.
class UnitName (u ∷ TempUnits) where
  unitName ∷ String

-- Now we are only allowed to define instances for `F` and `C`.
instance UnitName C where
  unitName ∷ String
  unitName = "C"

instance UnitName F where
  unitName ∷ String
  unitName = "F"

instance UnitName u ⇒ Show (Temp u) where
  show ∷ UnitName u ⇒ Temp u → String
  show (Temp t) = show t ++ "°" ++ unitName @u

unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName @u

-- >>> unit absoluteZero
-- "C"
--
-- >>> unit paperBurning
-- "F"
--
-- >>> absoluteZero
-- -273.15°C
--
-- >>> paperBurning
-- 451.0°F
--
-- >>> f2c paperBurning - absoluteZero
-- 505.92777777777775°C

printTemp ∷ ∀ u. UnitName u ⇒ Temp u → IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Unit: " ++ unit t

main ∷ IO ()
main = do
  printTemp paperBurning
  printTemp absoluteZero
  printTemp diff

-- Output:
-- Temperature: 451.0°F
-- Units: F
-- Temperature: -273.15°C
-- Units: C
-- Temperature: 505.92777777777775°C
-- Units: C
