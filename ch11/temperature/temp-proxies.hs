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
