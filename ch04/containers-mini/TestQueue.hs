import Data.Queue (dequeue, empty, enqueue, front, isEmpty)
import System.Exit (exitFailure)

main ∷ IO ()
main = do
  let q = enqueue 15 $ enqueue 10 $ enqueue 5 $ enqueue 0 empty -- q == front → 0, 5, 10, 15
      q' = dequeue $ dequeue q -- front → 10, 15
      q'' = enqueue 100 q' -- front → 10, 15, 100
      shouldBeTrue =
        [ front q' == Just 10,
          front q'' == Just 10,
          isEmpty $ dequeue $ dequeue q'
        ]
  if and shouldBeTrue then pure () else exitFailure
