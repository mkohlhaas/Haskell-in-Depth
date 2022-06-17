{-# LANGUAGE OverloadedStrings #-}

module GeoCoordsReq (getCoords) where

import App (MyApp)
import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), defaultHttpConfig, header, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import STExcept (SunInfoException (UnknownLocation), rethrowReqException)
import Types (Address, GeoCoords, WebAPIAuth (agent, email))

getCoords :: Address -> MyApp GeoCoords
getCoords addr = handle rethrowReqException $ do
  wauth <- ask
  let ep = https "nominatim.openstreetmap.org" /: "search" -- ep = end-point
      reqParams =
        mconcat
          [ "q" =: addr,
            "format" =: ("json" :: T.Text),
            "limit" =: (1 :: Int),
            "email" =: email wauth,
            header "User-Agent" (encodeUtf8 $ agent wauth)
          ]
      request = req GET ep NoReqBody jsonResponse reqParams
  res <- liftIO $ responseBody <$> runReq defaultHttpConfig request
  case res of
    [] -> throwM $ UnknownLocation addr
    (coords : _) -> pure coords
