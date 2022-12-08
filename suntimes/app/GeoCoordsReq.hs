{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GeoCoordsReq (getCoords) where

import App (MyApp, runMyApp)
import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req (GET (GET), JsonResponse, NoReqBody (NoReqBody), Option, Req, Scheme (Https), Url, defaultHttpConfig, header, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import STExcept (SunInfoException (UnknownLocation), rethrowReqException)
import Types (Address, GeoCoords, WebAPIAuth (..))

-- line 34:
-- "Thanks to the types, the req package knows that the HTTP response (array of objects in JSON) should be decoded as [GeoCoords].
--  This decoding is hidden inside the responseBody function. The decoding itself is possible via the FromJSON instance we defined in `Types.hs`.
--  `req` was also able to figure out that the JSON array should be decoded to a Haskell list."
getCoords ∷ Address → MyApp GeoCoords
getCoords addr = handle rethrowReqException $ do
  wauth ← ask
  let endPoint ∷ Url 'Https
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
    [] → throwM $ UnknownLocation addr -- Won't be `handle`d in line 19. `rethrowReqException` only covers HttpException's. It's in its signature!
    (coords : _) → pure coords

-- >>> runMyApp (getCoords "Hawaii") (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")
-- GeoCoords {lat = "19.593801499999998", lon = "-155.42837009716908", display_name = "Hawaii, United States"}
--
-- >>> runMyApp (getCoords "Rome") (WebAPIAuth "suntimes@chammy.info" "SunTimes.hs" "AG6C0TV2CAGE")
-- GeoCoords {lat = "41.8933203", lon = "12.4829321", display_name = "Roma, Roma Capitale, Lazio, Italia"}

