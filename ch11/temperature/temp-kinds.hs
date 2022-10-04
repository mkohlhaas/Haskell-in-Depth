{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

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

-- ghci> :set -XNoStarIsType -XDataKinds
-- ghci> :type F
-- F :: TempUnits
-- ghci> :kind F
-- F :: TempUnits
-- ghci> :kind TempUnits
-- TempUnits :: Type

-- ghci> paperBurning - absoluteZero
-- <interactive>:6:16: error: Couldn't match type ‘'C’ with ‘'F’

-- Sometimes we or the compiler need to disambiguate equally named values and corresponding promoted types.
-- In these situations, the ' prefix is used to mention a type and not a value.

-- `u` is a phantom type but only of a certain kind(!), i.e. TempUnits with the only possible types(!) `F` and `C`
newtype Temp (u ∷ TempUnits) = Temp Double
  deriving (Num, Fractional)

paperBurning ∷ Temp F
paperBurning = 451

absoluteZero ∷ Temp C
absoluteZero = -273.15

f2c ∷ Temp F → Temp C
f2c (Temp f) = Temp ((f -32) * 5 / 9)

-- Type Error: Expected kind ‘TempUnits’, but ‘Bool’ has kind ‘*’
-- nonsense ∷ Temp Bool
-- nonsense = Temp 0

-- Type Error: Couldn't match type ‘C’ with ‘F’
-- err = paperBurning - absoluteZero

diff ∷ Temp C
diff = f2c paperBurning - absoluteZero

-- We can also mention our new kind in type class declarations.
class UnitName (u ∷ TempUnits) where
  unitName ∷ String

-- Now we are not allowed to define instances for anything except `F` and `C`.
instance UnitName C where
  unitName = "C"

instance UnitName F where
  unitName = "F"

instance UnitName u ⇒ Show (Temp u) where
  show (Temp t) = show t ++ "°" ++ unitName @u

unit ∷ ∀ u. UnitName u ⇒ Temp u → String
unit _ = unitName @u

printTemp ∷ ∀ u. UnitName u ⇒ Temp u → IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Unit: " ++ unit t

main ∷ IO ()
main = do
  printTemp paperBurning
  printTemp absoluteZero
  printTemp diff
