-- The monadic ST type provides support for strict state threads.
--
-- ST s a -- returns a value of type `a` executing in thread `s`
--

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef)

comp1 ∷ ST s (STRef s Int)
comp1 = newSTRef 42

-- should not work and doesn't - reference stays hidden
-- >>> runST (comp2 (runST comp1))
-- Couldn't match type ‘s1’ with ‘s’
-- ...

-- should not work and doesn't - reference stays hidden
-- >>> runST comp1
-- Couldn't match type ‘a’ with ‘STRef s Int’
--   because type variable ‘s’ would escape its scope
-- ...

comp2 ∷ STRef s Int → ST s Int
comp2 = readSTRef

-- >>> runST (comp1 >>= comp2)
-- 42

main ∷ IO ()
main = print $ runST (comp1 >>= comp2) -- 42
