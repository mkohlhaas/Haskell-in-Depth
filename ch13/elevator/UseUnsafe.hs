import Control.Monad (foldM_)
import Data.Coerce (coerce)
import Elevator.Unsafe (DoorState (Closed), Elevator (Elevator), Floor (..), call)
import System.Environment (getArgs)

-- gf = ground floor
gfElevator ∷ Elevator
gfElevator = Elevator (Floor 0) Closed

-- $ cabal run unsafe-elevator 1 2 3
-- Call to: Floor 1 of 5.
-- Going up.
-- Door is opening.
-- Elevator {current = Floor 1 of 5, door = Opened}.
-- Call to: Floor 2 of 5.
-- Door is closing.
-- Going up.
-- Door is opening.
-- Elevator {current = Floor 2 of 5, door = Opened}
-- Call to: Floor 3 of 5.
-- Door is closing.
-- Going up.
-- Door is opening.
-- Elevator {current = Floor 3 of 5, door = Opened}

-- $ cabal run unsafe-elevator 5
-- Call to: Floor 5 of 5.
-- Going up.
-- Going up.
-- Going up.
-- Going up.
-- Going up.
-- Door is opening.
-- Elevator {current = Floor 5 of 5, door = Opened}

-- $ cabal run unsafe-elevator 6
-- Call to: Floor 6 of 5.
-- Going up.
-- Going up.
-- Going up.
-- Going up.
-- Going up.
-- unsafe-elevator: Elevator on the top floor.
-- CallStack (from HasCallStack):
--   error, called at elevator/Elevator/Unsafe.hs:86:17 in main:Elevator.Unsafe

main ∷ IO ()
main = do
  floors ← map read <$> getArgs
  foldM_ traceTo gfElevator (map Floor floors)
  where
    prt ∷ Show b ⇒ b → IO b
    prt el = print el >> pure el
    traceTo ∷ Elevator → Floor → IO Elevator
    traceTo el fl = call fl el >>= prt
