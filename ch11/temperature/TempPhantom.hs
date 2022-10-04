{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TempPhantom where

-- 'unit' is called a phantom type - this type parameter doesn't appear on the RHS
-- Deriving Num and Fractional allows to use basic arithmetic (+, -, *, /) for Temp.
-- Temp is a Double. There can be different kind of temperatures, e.g. Celsius, Fahrenheit, Kelvin, which shouldn't be mixed.
newtype Temp unit = Temp Double
  deriving (Num, Fractional)

-- empty declarations - no values
-- Creates C, F and K types.
data C -- Celsius
data F -- Fahrenheit
data K -- Kelvin

paperBurning ∷ Temp F
paperBurning = 451

absoluteZero ∷ Temp C
absoluteZero = -273.15

f2c ∷ Temp F → Temp C
f2c (Temp f) = Temp ((f -32) * 5 / 9)

-- Type Error: Couldn't match type ‘C’ with ‘F’
-- err = paperBurning - absoluteZero

diff ∷ Temp C
diff = f2c paperBurning - absoluteZero

-- This is allowed but doesn't make sense.
nonsence ∷ Temp Bool
nonsence = 0
