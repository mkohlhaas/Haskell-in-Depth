{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Stretegies:
-- STOCK    refers to specific type classes with deriving algorithms built into GHC.
-- NEWTYPE  supports generating instances for newtype declarations.
--          The code for those instances is the same as the code for the underlying types.
-- ANYCLASS strategy allows us to derive empty instances.
--          No methods are generated for them.
--          This makes sense if we have default implementations in type class definitions.
newtype Age = Age {age ∷ Int}
  -- deriving (Show, Generic, Num, ToJSON)
  deriving stock (Show, Generic)
  deriving newtype (Num) ------ Age can be seen as a Num
  deriving anyclass (ToJSON) -- Age can be seen as JSON

theAge ∷ Age
theAge = 33

-- >>> theAge
-- Age {age = 33}

-- >>> encode theAge
-- "{\"age\":33}"

-- >>> theAge + 5
-- Age {age = 38}

main ∷ IO ()
main = do
  print theAge ----------- Age {age = 33}
  print $ encode theAge -- "{\"age\":33}"
