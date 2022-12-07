{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GeoCoordsReq (getCoords) where

import App (MyApp)
import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), defaultHttpConfig, header, https, jsonResponse, req, responseBody, runReq, (/:), (=:), Url, Scheme (Https), Option, Req, JsonResponse)
import STExcept (SunInfoException (UnknownLocation), rethrowReqException)
import Types (Address, GeoCoords, WebAPIAuth (agent, email))
import Debug.Trace (trace)

-- "Thanks to the types, the req package knows that the HTTP
--  response (array of objects in JSON) should be decoded as [GeoCoords]. This decoding
--  is hidden inside the responseBody function. The decoding itself is possible via the
--  FromJSON instance we defined earlier. Note that req was able to figure out that the
--  JSON array should be decoded to the Haskell list."
getCoords ∷ Address → MyApp GeoCoords
getCoords addr = handle rethrowReqException $ do
  wauth ← ask
  let
      endPoint ∷ Url 'Https
      endPoint = https "nominatim.openstreetmap.org" /: "search"
      reqParams ∷ Option scheme
      reqParams =
        mconcat
          [ "q" =: addr,
            "format" =: ("json" ∷ T.Text),
            "limit" =: (1 ∷ Int),
            "email" =: email wauth,
            header "User-Agent" (encodeUtf8 $ agent wauth)
          ]
      request ∷ Req (JsonResponse [GeoCoords])
      request = req GET endPoint NoReqBody jsonResponse reqParams
  res ← liftIO $ responseBody <$> runReq defaultHttpConfig request
  case res of
    [] → throwM $ UnknownLocation addr -- Won't `handle` in line 14 catch this and call rethrowReqException? No! rethrowReqException only covers HttpException's. It's in its signature!
    (coords : _) → pure coords
