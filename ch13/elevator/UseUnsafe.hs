import Control.Monad
import Elevator.Unsafe
import System.Environment

gfElevator :: Elevator
gfElevator = Elevator (Floor 0) Closed

main :: IO ()
main = do
  floors <- map read <$> getArgs
  foldM_ traceTo gfElevator (map Floor floors)
  where
    prt el = print el >> pure el
    traceTo el fl = call fl el >>= prt
