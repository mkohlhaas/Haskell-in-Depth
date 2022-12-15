{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Capture, Get, JSON, Proxy (..), Server, serve, type (:<|>) (..), type (:>))
import Servant.HTML.Blaze (HTML)
import qualified Text.Blaze.Html5 as H

data Rating = Bad | Good | Great
  deriving (Show, Generic, ToJSON)

data ServiceStatus = Ok | Down
  deriving (Show, Generic, ToJSON)

type BookID = Int

type BookInfoAPI =
  Get '[JSON] ServiceStatus
    :<|> "title" :> Capture "id" BookID :> Get '[HTML] H.Html
    :<|> "year" :> Capture "id" BookID :> Get '[JSON] Int
    :<|> "rating" :> Capture "id" BookID :> Get '[JSON] Rating

-- >>> :kind! Server BookInfoAPI
-- Server BookInfoAPI ∷ *
-- = Handler ServiceStatus
--   :<|> ((Int → Handler (MarkupM ()))
--   :<|> ((Int → Handler Int)
--   :<|>  (Int → Handler Rating)))

impl ∷ Server BookInfoAPI
impl =
  pure Ok
    :<|> title
    :<|> year
    :<|> rating
  where
    title _ = pure $ H.b "Haskell in Depth"
    year _ = pure 2021
    rating _ = pure Great

app ∷ Application
app = serve (Proxy ∷ Proxy BookInfoAPI) impl

main ∷ IO ()
main = run 8081 app

-- $ cabal run api-servant
-- $ curl http://localhost:8081/            ⇒ "Ok"
-- $ curl http://localhost:8081/title/1234  ⇒ <b>Haskell in Depth</b>
-- $ curl http://localhost:8081/year/1234   ⇒ 2021
-- $ curl http://localhost:8081/rating/1234 ⇒ "Great"
