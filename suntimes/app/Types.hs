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

-- for nominatim.openstreetmap.org
-- https://nominatim.org/release-docs/develop/
-- given a city name get its coordinates
data GeoCoords = GeoCoords
  { lat :: Text,
    lon :: Text
  }
  deriving (Show, Generic, FromJSON)

-- for api.sunrise-sunset.org
-- given the coordinates of the city get its sunrise and sunset times in UTC
data SunTimes dt = SunTimes
  { sunrise :: dt,
    sunset :: dt
  }
  deriving (Show, Generic, FromJSON)

data WebAPIAuth = WebAPIAuth
  { timeZoneDBkey :: Text, -- for api.timezonedb.com -> given UTC and timezone get the local time
    email :: Text, -- for nominatim.openstreetmap.org
    agent :: Text -- for nominatim.openstreetmap.org
  }
  deriving (Show, Generic, FromJSON)
