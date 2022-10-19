{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving (Show)

data ServiceStatus = Ok | Down
  deriving (Show)

data Get (a ∷ Type)

data Capture (a ∷ Type)

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a ∷ k) :> (b ∷ Type)

infixr 9 :>

type BookID = Int

type BookInfoAPI =
  Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String
    :<|> "year" :> Capture BookID :> Get Int
    :<|> "rating" :> Capture BookID :> Get Rating

type HandlerAction a = IO a

type family Server layout ∷ Type

type instance Server (Get a) = HandlerAction a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s ∷ Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a → Server r

impl1 ∷ Server BookInfoAPI
impl1 =
  pure Ok
    :<|> title
    :<|> year
    :<|> rating
  where
    title _ = pure "Haskell in Depth"
    year _ = pure 2021
    rating _ = pure Great

impl2 ∷ Server BookInfoAPI
impl2 =
  pure Down
    :<|> title
    :<|> year
    :<|> rating
  where
    notImplemented = ioError (userError "not implemented")
    title _ = notImplemented
    year _ = notImplemented
    rating _ = notImplemented

encode ∷ Show a ⇒ IO a → IO String
encode m = show <$> m

type Request = [String]

-- Routing can also be constructed automatically from an interface.
class HasServer layout where
  route ∷ Proxy layout → Server layout → Request → Maybe (IO String)

instance Show a ⇒ HasServer (Get a) where
  route ∷ Proxy (Get a) → HandlerAction a → Request → Maybe (IO String)
  route _ handler [] = Just $ encode handler
  route _ _ _ = Nothing

instance {-# OVERLAPS #-} HasServer (Get String) where
  route ∷ Proxy (Get String) → IO String → Request → Maybe (IO String)
  route _ handler [] = Just handler
  route _ _ _ = Nothing

instance (HasServer a, HasServer b) ⇒ HasServer (a :<|> b) where
  route ∷ Proxy (a :<|> b) → (Server a :<|> Server b) → Request → Maybe (IO String)
  route _ (handlera :<|> handlerb) xs = route (Proxy ∷ Proxy a) handlera xs <|> route (Proxy ∷ Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) ⇒ HasServer ((s ∷ Symbol) :> r) where
  route ∷ Proxy (s :> r) → Server r → Request → Maybe (IO String)
  route _ handler (x : xs)
    | symbolVal (Proxy ∷ Proxy s) == x = route (Proxy ∷ Proxy r) handler xs
  route _ _ _ = Nothing

instance (Read a, HasServer r) ⇒ HasServer (Capture a :> r) where
  route ∷ Proxy (Capture a :> r) → (a → Server r) → [String] → Maybe (IO String)
  route _ handler (x : xs) = do
    a ← readMaybe x
    route (Proxy ∷ Proxy r) (handler a) xs
  route _ _ _ = Nothing

get ∷ HasServer layout ⇒ Proxy layout → Server layout → [String] → IO String
get proxy handler request = case route proxy handler request of
  Nothing → ioError (userError "404")
  Just m → m

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["title", "7548"]
-- "Haskell in Depth"

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["year", "7548"]
-- "2021"

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["rating", "7548"]
-- "Great"

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["title", "7548"]
-- user error (not implemented)

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["year", "7548"]
-- user error (not implemented)

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["rating", "7548"]
-- user error (not implemented)

check ∷ Server BookInfoAPI → IO ()
check impl = do
  b ← get (Proxy ∷ Proxy BookInfoAPI) impl []
  answer ← get (Proxy ∷ Proxy BookInfoAPI) impl ["year", "7548"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = check impl1
