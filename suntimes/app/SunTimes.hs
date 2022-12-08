{-# LANGUAGE DataKinds #-}
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
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), defaultHttpConfig, http, https, jsonResponse, req, responseBody, runReq, (/:), (=:), Url, Scheme (Https, Http), Option)
import STExcept (SunInfoException (ServiceAPIError, UnknownTime), rethrowReqException)
import Types (GeoCoords (..), SunTimes (..), WebAPIAuth (timeZoneDBkey), When (..))
import System.Console.GetOpt (getOpt)

newtype SunTimesWrapper dt = SunTimesWrapper {results ∷ SunTimes dt}
  deriving (Show, Generic, FromJSON)

getSunTimesUTC ∷ GeoCoords → When → MyApp (SunTimes UTCTime)
getSunTimesUTC GeoCoords {..} when = handle rethrowReqException $
  liftIO $
    runReq defaultHttpConfig $ do
      r ← req GET endPoint NoReqBody jsonResponse reqParams
      pure (results $ responseBody r)
  where
    endPoint ∷ Url 'Https
    endPoint = https "api.sunrise-sunset.org" /: "json"
    reqParams ∷ Option 'Https
    reqParams =
      mconcat $
        [ "lat" =: lat,
          "lng" =: lon,
          "formatted" =: (0 ∷ Int)
        ]
          ++ whenToOptions when
    whenToOptions ∷ When → [Option 'Https]
    whenToOptions Now = []
    whenToOptions (On day) = ["date" =: formatTime defaultTimeLocale "%Y-%m-%d" day]

getSunTimes ∷ GeoCoords → When → MyApp (SunTimes ZonedTime)
getSunTimes getCoords atDay = do
  SunTimes {..} ← getSunTimesUTC getCoords atDay `catch` noTimeHandler
  ltz ← lookupTimeZone getCoords sunrise `catchAll` const (pure utc)
  pure $ SunTimes (utcToZonedTime ltz sunrise) (utcToZonedTime ltz sunset)
  where
    noTimeHandler ∷ MonadThrow m ⇒ SunInfoException → m a
    noTimeHandler (ServiceAPIError _) = throwM (UnknownTime getCoords)
    noTimeHandler e = throwM e

data TimeZoneInfo = TimeZoneInfo
  { gmtOffset ∷ !Int,
    abbreviation ∷ !String,
    dst ∷ !String
  }
  deriving (Show, Generic, FromJSON)

lookupTimeZone ∷ GeoCoords → UTCTime → MyApp TimeZone
lookupTimeZone GeoCoords {..} t = do
  key ← asks timeZoneDBkey
  let
      ep ∷ Url 'Http
      ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
      reqParams ∷ Option 'Http
      reqParams =
        mconcat
          [ "key" =: key,
            "lat" =: lat,
            "lng" =: lon,
            "time" =: formatTime defaultTimeLocale "%s" t,
            "format" =: ("json" ∷ T.Text),
            "fields" =: ("gmtOffset,abbreviation,dst" ∷ T.Text),
            "by" =: ("position" ∷ T.Text)
          ]
  r ← liftIO $ runReq defaultHttpConfig $ req GET ep NoReqBody jsonResponse reqParams
  pure (timeZoneInfo2TimeZone $ responseBody r)
  where
    secondsToTimeZone ∷ Int → TimeZone
    secondsToTimeZone s = minutesToTimeZone (s `div` 60)
    timeZoneInfo2TimeZone ∷ TimeZoneInfo → TimeZone
    timeZoneInfo2TimeZone TimeZoneInfo {..} =
      (secondsToTimeZone gmtOffset)
        { timeZoneName = abbreviation,
          timeZoneSummerOnly = dst == "1"
        }
