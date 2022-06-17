{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ProcessRequest (processMany, processInteractively) where

import App (MyApp)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM), finally)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (TimeLocale, ZonedTime, defaultTimeLocale, formatTime, parseTimeM)
import GeoCoordsReq (getCoords)
import STExcept (RequestError (..), SunInfoException (FormatError, NetworkError, ServiceAPIError))
import SunTimes (getSunTimes)
import Types (GeoCoords (display_name), SunTimes (..), When (..))

-- [<date>@]<address/location>
parseRequestLine :: Text -> Either RequestError (Text, When)
parseRequestLine txt = parse $ split txt
  where
    split t = case T.breakOn "@" t of
      (addr, "") -> ("", addr)
      (day, addr) -> (T.strip day, T.strip $ T.tail addr)
    parse (_, "") = Left EmptyRequest
    parse ("", addr) = Right (addr, Now)
    parse (d, addr) =
      case parseTimeM False defaultTimeLocale "%Y-%-m-%-d" (T.unpack d) of
        Nothing -> Left (WrongDay d)
        Just d' -> Right (addr, On d')

formatResult :: Text -> SunTimes ZonedTime -> TimeLocale -> Text
formatResult req SunTimes {..} loc =
  mconcat [day, " @ ", req, ":\n  Sunrise: ", fmt sunrise, "\n  Sunset: ", fmt sunset]
  where
    day = T.pack $ formatTime loc "%x" sunrise
    fmt t = T.pack $ formatTime loc "%X %Z" t

processRequest :: Text -> MyApp Text
processRequest t = processR $ parseRequestLine $ T.strip t
  where
    processR (Left e) = throwM (FormatError e)
    processR (Right (addr, day)) = do
      coords <- getCoords addr
      st <- getSunTimes coords day
      pure $ formatResult (display_name coords) st defaultTimeLocale

processMany :: [Text] -> MyApp ()
processMany = mapM_ processRequestWrapper
  where
    processRequestWrapper r =
      unless ("#" `T.isPrefixOf` r) $ -- every request that starts with '#' is a comment
        (processRequest r >>= liftIO . TIO.putStrLn) `catch` handler r `finally` delaySec 1
    delaySec sec = liftIO $ threadDelay (sec * 1000000)
    handler :: Text -> SunInfoException -> MyApp ()
    handler r e = liftIO $ TIO.putStrLn $ "Error in request '" <> r <> "': " <> T.pack (show e)

processInteractively :: MyApp ()
processInteractively = action `catch` handler
  where
    action = do
      liftIO $ TIO.putStrLn "Enter your request:"
      req <- liftIO TIO.getLine
      res <- processRequest req
      liftIO $ TIO.putStrLn res

    -- `handler` only catches SunInfoException's
    handler :: SunInfoException -> MyApp ()
    handler e@(ServiceAPIError _) = liftIO $ print e
    handler e@(NetworkError _) = liftIO $ print e
    handler e = do
      liftIO $
        TIO.putStr $
          "There was an error while processing your request: "
            <> T.pack (show e)
            <> "\nDo you want to try again (Y/N)?"
      yesno <- liftIO TIO.getLine
      when (yesno `elem` ["y", "Y", "yes"]) processInteractively
