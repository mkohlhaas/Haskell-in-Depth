{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SunTimes (getSunTimes) where

import App (MyApp)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (..), catchAll, handle)
import Control.Monad.Reader (MonadIO (liftIO), asks)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Data.Time (TimeZone (timeZoneName, timeZoneSummerOnly), UTCTime, ZonedTime, defaultTimeLocale, formatTime, minutesToTimeZone, utc, utcToZonedTime)
import GHC.Generics (Generic)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), defaultHttpConfig, http, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import STExcept (SunInfoException (ServiceAPIError, UnknownTime), rethrowReqException)
import Types (GeoCoords (..), SunTimes (..), WebAPIAuth (timeZoneDBkey), When (..))

newtype SunTimesWrapper dt = SunTimesWrapper {results :: SunTimes dt}
  deriving (Show, Generic, FromJSON)

getSunTimesUTC :: GeoCoords -> When -> MyApp (SunTimes UTCTime)
getSunTimesUTC GeoCoords {..} w = handle rethrowReqException $
  liftIO $
    runReq defaultHttpConfig $ do
      r <- req GET ep NoReqBody jsonResponse reqParams
      pure (results $ responseBody r)
  where
    ep = https "api.sunrise-sunset.org" /: "json" -- ep = end-point
    reqParams =
      mconcat $
        [ "lat" =: lat,
          "lng" =: lon,
          "formatted" =: (0 :: Int)
        ]
          ++ whenToOptions w
    whenToOptions Now = []
    whenToOptions (On day) = ["date" =: formatTime defaultTimeLocale "%Y-%m-%d" day]

getSunTimes :: GeoCoords -> When -> MyApp (SunTimes ZonedTime) -- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html#t:ZonedTime
getSunTimes gc d = do
  SunTimes {..} <- getSunTimesUTC gc d `catch` noTimeHandler
  ltz <- lookupTimeZone gc sunrise `catchAll` const (pure utc) -- if lookup of timezone fails just use UTC timezone (https://hackage.haskell.org/package/time-1.13/docs/Data-Time-LocalTime.html#v:utc)
  pure $ SunTimes (utcToZonedTime ltz sunrise) (utcToZonedTime ltz sunset)
  where
    noTimeHandler :: MonadThrow m => SunInfoException -> m a
    noTimeHandler (ServiceAPIError _) = throwM (UnknownTime gc)
    noTimeHandler e = throwM e

data TimeZoneInfo = TimeZoneInfo
  { gmtOffset :: Int,
    abbreviation :: String,
    dst :: String
  }
  deriving (Show, Generic, FromJSON)

lookupTimeZone :: GeoCoords -> UTCTime -> MyApp TimeZone
lookupTimeZone GeoCoords {..} t = do
  key <- asks timeZoneDBkey
  let ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
      reqParams =
        mconcat
          [ "key" =: key,
            "lat" =: lat,
            "lng" =: lon,
            "time" =: formatTime defaultTimeLocale "%s" t,
            "format" =: ("json" :: T.Text),
            "fields" =: ("gmtOffset,abbreviation,dst" :: T.Text),
            "by" =: ("position" :: T.Text)
          ]
  r <-
    liftIO $
      runReq defaultHttpConfig $
        req GET ep NoReqBody jsonResponse reqParams
  pure (timeZoneInfo2TimeZone $ responseBody r)
  where
    secondsToTimeZone s = minutesToTimeZone (s `div` 60)
    timeZoneInfo2TimeZone TimeZoneInfo {..} =
      (secondsToTimeZone gmtOffset)
        { timeZoneName = abbreviation,
          timeZoneSummerOnly = dst == "1"
        }
