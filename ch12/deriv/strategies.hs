{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- STOCK strategy refers to specific type classes with deriving algorithms built into GHC.
-- NEWTYPE strategy supports generating instances for newtype declarations. The code for those instances is the same as the code for the underlying types.
-- ANYCLASS strategy allows us to derive empty instances. No methods are generated for them. This makes more sense if we have default implementations in type class definitions.
newtype Age = Age {age ∷ Int}
  -- deriving (Show, Generic, Num, ToJSON) -- Although GHC has an algorithm to choose a strategy to be used by default, it's safer to avoid it.
  deriving stock (Show, Generic)
  deriving newtype (Num)
  deriving anyclass (ToJSON)

theAge ∷ Age
theAge = 33

main ∷ IO ()
main = do
  print theAge
  print $ encode theAge
