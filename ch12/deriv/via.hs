{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE StandaloneDeriving #-}

import Data.Monoid

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- We give GHC an example of a type that already implements the instance we need and say:
-- "Generate an implementation in the same way but replace types with ours!"
newtype Age' = Age' Int
  deriving (Eq, Ord) via Int

-- Alt prefers the first available not-Nothing value.
-- Dual prefers the last available not-Nothing value by swapping arguments to `<>`.

-- Deriving Semigroup and Monoid instances with a newtype strategy requires having them for Int (that is the default for Maybe a). Unfortunately, there are none.

-- With DerivingVia we can instruct GHC to follow the Alt Maybe Int example.
newtype MAge = MAge (Maybe Int)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Alt Maybe Int)

-- standalone deriving with {-# LANGUAGE StandaloneDeriving #-} works
-- deriving via (Alt Maybe Int) instance Semigroup MAge
-- deriving via (Alt Maybe Int) instance Monoid MAge

-- If we prefer to take the last available not-Nothing value, we go with Dual.
-- deriving (Semigroup, Monoid) via (Dual (Alt Maybe Int)) -- use the Alt instance but swap arguments

main ∷ IO ()
main = putStrLn "Just a dummy main."
