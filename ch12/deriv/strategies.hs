{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

newtype Age = Age {age :: Int}
  -- deriving (Show, Generic, Num, ToJSON)
  deriving stock (Show, Generic)
  deriving newtype (Num)
  deriving anyclass (ToJSON)

theAge :: Age
theAge = 33

main :: IO ()
main = do
  print theAge
  print $ encode theAge
