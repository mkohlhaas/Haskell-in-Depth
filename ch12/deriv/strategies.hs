{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Strategies:
-- STOCK    refers to specific type classes with deriving algorithms built into GHC.
-- NEWTYPE  supports generating instances for newtype declarations.
--          The code for those instances is the same as the code for the underlying types.
-- ANYCLASS strategy allows us to derive empty instances.
--          No methods are generated for them.
--          This makes sense if we have default implementations in type class definitions.
-- VIA      strategy allows us to give an example of how the code should be generated. A generalization of the newtype strategy.

-- GHC can derive following classes:
--  1. Eq
--  2. Ord
--  3. Enum
--  4. Ix
--  5. Bounded
--  6. Read
--  7. Show
--  8. Generic and Generic1 with DeriveGeneric
--  9. Functor with DeriveFunctor.
-- 10. Foldable with DeriveFoldable
-- 11. Traversable with DeriveTraversable
-- 12. Typeable and Data with DeriveDataTypeable
-- 13. Lift with DeriveLift (for TH)

newtype Age = Age {age ∷ Int}
  -- deriving (Show, Generic, Num, ToJSON) -- GHC has an algorithm for choosing a strategy to be used, but it's safer to avoid it.
  deriving stock (Show, Generic)
  deriving newtype (Num) ------ use Age as a Number
  deriving anyclass (ToJSON) -- use Age as JSON

-- using Age as a Number
theAge ∷ Age
theAge = 33

-- >>> theAge
-- Age {age = 33}

-- JSON
-- >>> encode theAge
-- "{\"age\":33}"

-- using Age as a Number
-- >>> theAge + 5
-- Age {age = 38}

main ∷ IO ()
main = do
  print theAge ----------- Age {age = 33}
  print $ encode theAge -- "{\"age\":33}"
