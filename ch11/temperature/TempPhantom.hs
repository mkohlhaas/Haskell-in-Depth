{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TempPhantom where

-- 'unit' is called a phantom type - this type parameter doesn't appear on the RHS.
-- Deriving Num and Fractional allows to use basic arithmetic (+, -, *, /) for Temp.
-- Temp is a Double. There can be different kind of temperatures, e.g. Celsius, Fahrenheit, Kelvin, which shouldn't be mixed.
newtype Temp unit = Temp Double
  -- deriving (Num, Fractional, Show)
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

-- >>> paperBurning
-- Temp 451.0

-- >>> paperBurning - paperBurning
-- Temp 0.0

-- >>> absoluteZero
-- Temp (-273.15)

f2c ∷ Temp F → Temp C
f2c (Temp f) = Temp $ (f -32) * 5 / 9

-- >>> f2c <$> [70, 75, 80, 85, 90, 99]
-- [Temp 21.11111111111111,Temp 23.88888888888889,Temp 26.666666666666668,Temp 29.444444444444443,Temp 32.22222222222222,Temp 37.22222222222222]

-- Type Error: Couldn't match type ‘C’ with ‘F’
-- err = paperBurning - absoluteZero

diff ∷ Temp C
diff = f2c paperBurning - absoluteZero

-- >>> diff
-- Temp 505.92777777777775

-- This is allowed but doesn't make sense.
nonsence ∷ Temp Bool
nonsence = 0

-- >>> nonsence
-- Temp 0.0

