{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ProcessRequest {- (processMany, processInteractively) -} where

import App (MyApp, runMyApp)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM), finally)
import Control.Monad.Logger (logInfoN)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (TimeLocale, ZonedTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Format (FormatTime)
import GeoCoordsReq (getCoords)
import STExcept (RequestError (..), SunInfoException (FormatError, NetworkError, ServiceAPIError))
import SunTimes (getSunTimes)
import Types (Address, GeoCoords (display_name), SunTimes (..), WebAPIAuth (..), When (..))

-- request format: [<date>@]<location>
parseRequestLine ∷ Text → Either RequestError (Text, When)
parseRequestLine req = parse $ split req
  where
    split ∷ Text → (Text, Text)
    split t = case T.breakOn "@" t of
      (addr, "") → ("", addr)
      (day, addr) → (T.strip day, T.strip $ T.tail addr)

    parse ∷ (Eq a, IsString a) ⇒ (Text, a) → Either RequestError (a, When)
    parse (_, "") = Left EmptyRequest
    parse ("", addr) = Right (addr, Now)
    parse (d, addr) =
      maybe
        (Left $ WrongDay d)
        (\day → Right (addr, On day))
        (parseTimeM False defaultTimeLocale "%Y-%-m-%-d" $ T.unpack d)

-- >>> parseRequestLine "1966-04-18@Paris"
-- Right ("Paris",On 1966-04-18)
--
-- >>> parseRequestLine "18-04-1966@Paris"
-- Left (WrongDay "18-04-1966")

formatResult ∷ Text → SunTimes ZonedTime → TimeLocale → Text
formatResult location SunTimes {..} locale =
  mconcat [day, " @ ", location, ":\n  Sunrise: ", fmt sunrise, "\n  Sunset: ", fmt sunset]
  where
    day ∷ Text
    day = T.pack $ formatTime locale "%x" sunrise
    fmt ∷ FormatTime t ⇒ t → Text
    fmt t = T.pack $ formatTime locale "%X %Z" t

processRequest ∷ Text → MyApp Text
processRequest req = processR $ parseRequestLine $ T.strip req
  where
    processR ∷ Either RequestError (Address, When) → MyApp Text
    processR (Left e) = throwM (FormatError e)
    processR (Right (addr, day)) = do
      coords ← getCoords addr
      logInfoN $ T.pack $ "Coordinates: " <> show coords
      st ← getSunTimes coords day
      pure $ formatResult (display_name coords) st defaultTimeLocale

-- >>> runMyApp (processRequest "1966-04-18@Paris") (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")
-- "04/18/66 @ Paris, \206le-de-France, France m\233tropolitaine, France:\n  Sunrise: 04:52:52 UTC\n  Sunset: 18:47:26 UTC"
--
-- >>> runMyApp (processRequest "Singapore") (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")
-- "12/07/22 @ Singapore:\n  Sunrise: 22:53:57 UTC\n  Sunset: 10:58:59 UTC"

processMany ∷ [Text] → MyApp ()
processMany = mapM_ processRequestWrapper
  where
    processRequestWrapper ∷ Text → MyApp ()
    processRequestWrapper req =
      unless ("#" `T.isPrefixOf` req) $ -- '#' is a comment line
        (processRequest req >>= liftIO . TIO.putStrLn) `catch` handler req `finally` delaySec 1
    handler ∷ Text → SunInfoException → MyApp ()
    handler req excpt = liftIO $ TIO.putStrLn $ "Error in request '" <> req <> "': " <> T.pack (show excpt)
    delaySec ∷ Int → MyApp ()
    delaySec sec = liftIO $ threadDelay (sec * 1000000)

-- paste this into ghci
-- runMyApp (processMany ["1966-04-18@Paris", "Singapore"]) (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")

processInteractively ∷ MyApp ()
processInteractively = action `catch` handler
  where
    action ∷ MyApp ()
    action = do
      liftIO $ TIO.putStrLn "Enter your request:"
      req ← liftIO TIO.getLine
      res ← processRequest req
      liftIO $ TIO.putStrLn res
      processInteractively
    handler ∷ SunInfoException → MyApp ()
    handler e@(ServiceAPIError _) = liftIO $ print e
    handler e@(NetworkError _) = liftIO $ print e
    handler e = do
      liftIO $ TIO.putStrLn $ "There was an error while processing your request: " <> T.pack (show e) <> "."
      processInteractively

-- paste this into ghci
-- runMyApp processInteractively (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")

