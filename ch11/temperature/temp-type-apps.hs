import TempPhantom (Temp, absoluteZero, diff, paperBurning)
import UnitNameTypeApps (UnitName, unit)

printTemp ∷ UnitName u ⇒ Temp u → IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Units: " ++ unit t

-- tempStr ∷ UnitName u ⇒ Temp u → IO ()
tempStr ∷ UnitName u ⇒ Temp u → [Char]
tempStr t = "Temperature: " <> show t <> unit t

-- >>> tempStr paperBurning
-- "Temperature: 451.0\176FF"

-- >>> tempStr absoluteZero
-- "Temperature: -273.15\176CC"

-- >>> tempStr diff
-- "Temperature: 505.92777777777775\176CC"

main ∷ IO ()
main = do
  printTemp paperBurning
  printTemp absoluteZero
  printTemp diff
