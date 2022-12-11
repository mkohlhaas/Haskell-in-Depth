import TempPhantom (Temp, absoluteZero, diff, paperBurning)
import UnitNameProxies (UnitName (..), unit)

printTemp ∷ UnitName u ⇒ Temp u → IO ()
printTemp t = do
  putStrLn $ "Temperature: " ++ show t
  putStrLn $ "Units: " ++ unit t

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
