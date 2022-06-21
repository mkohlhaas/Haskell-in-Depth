{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

data TempUnits = F | C

newtype Temp (u :: TempUnits) = Temp Double
  deriving (Num, Fractional)

paperBurning :: Temp F
paperBurning = 451

absoluteZero :: Temp C
absoluteZero = -273.15

f2c :: Temp F -> Temp C
f2c (Temp f) = Temp ((f -32) * 5 / 9)

-- TYPE ERROR: Expected kind ‘TempUnits’, but ‘Bool’ has kind ‘*’
-- nonsense :: Temp Bool
-- nonsense = Temp 0

-- TYPE ERROR: Couldn't match type ‘C’ with ‘F’
-- err = paperBurning - absoluteZero

diff :: Temp C
diff = f2c paperBurning - absoluteZero

class UnitName (u :: TempUnits) where
  unitName :: String

instance UnitName C where
  unitName = "C"

instance UnitName F where
  unitName = "F"

instance UnitName u => Show (Temp u) where
  show (Temp t) = show t ++ "°" ++ unitName @u

unit :: ∀ u. UnitName u => Temp u -> String
unit _ = unitName @u

printTemp :: ∀ u. UnitName u => Temp u -> IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Unit: " ++ unit t

main :: IO ()
main = do
  printTemp paperBurning
  printTemp absoluteZero
  printTemp diff
