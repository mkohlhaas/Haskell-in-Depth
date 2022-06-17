{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

type Address = Text

data When = Now | On Day
  deriving (Show)

data GeoCoords = GeoCoords
  { lat :: Text,
    lon :: Text
  }
  deriving (Show, Generic, FromJSON)

data SunTimes dt = SunTimes
  { sunrise :: dt,
    sunset :: dt
  }
  deriving (Show, Generic, FromJSON)

data WebAPIAuth = WebAPIAuth
  { timeZoneDBkey :: Text,
    email :: Text,
    agent :: Text
  }
  deriving (Show, Generic, FromJSON)
