{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving (Show)

data ServiceStatus = Ok | Down
  deriving (Show)

type HandlerAction a = IO a

data Get a

data Capture a

data a :<|> b = a :<|> b

infixr 8 :<|>

data a :> b

infixr 9 :>

---------------------------------------------------------
--         Type family to pattern match kinds!         --
--         Pattern matching on the type-level.         --
---------------------------------------------------------

-- Server ... ≌ implementation

type family Server api ∷ Type

type instance Server (Get a) = HandlerAction a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s ∷ Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a → Server r

---------------------------------------------------------

type BookID = Int

-- consists of four request handlers
type BookInfoAPI =
  Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String ----- Symbols "title", "year", "rating" are necessary bc we match on them in line 135 (symbolVal)
    :<|> "year" :> Capture BookID :> Get Int --------- to find the right request handler
    :<|> "rating" :> Capture BookID :> Get Rating

-- >>> :kind! Server BookInfoAPI
-- Server BookInfoAPI ∷ Type
-- = HandlerAction ServiceStatus ---------------------- root
--   :<|> ((Int → HandlerAction String) --------------- title
--   :<|> ((Int → HandlerAction Int) ------------------ year
--   :<|>  (Int → HandlerAction Rating))) ------------- rating

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
    notImplemented = fail "not implemented"
    title _ = notImplemented
    year _ = notImplemented
    rating _ = notImplemented

encode ∷ Show a ⇒ IO a → IO String
encode m = show <$> m

type Request = [String]

--------------------------------------------------------------------------------------------------------------------
--                              Routing can also be constructed from an interface                                 --
--------------------------------------------------------------------------------------------------------------------

-- compare to previous version of route: (they are basically the same)
-- route ∷                Server BookInfoAPI → Request → Maybe (HandlerAction String)
-- route ∷ Proxy api →    Server api         → Request → Maybe (HandlerAction String)
class HasServer api where
  route ∷ Proxy api → Server api → Request → Maybe (HandlerAction String)

-- We have to do the type conversion from the type family ourselves.
-- This won't work:
-- instance Show a ⇒ HasServer (Get a) where
--   route ∷ Proxy (Get a) → Get a → Request → Maybe (HandlerAction String)
--                           ^^^^^
--   route _ handler [] = Just $ encode handler
--   route _ _ _ = Nothing

instance Show a ⇒ HasServer (Get a) where
  route ∷ Proxy (Get a) → HandlerAction a → Request → Maybe (HandlerAction String)
  route _ handlerAction [] = Just $ encode handlerAction
  route _ _ _ = Nothing

-- avoid escaping for Strings
instance {-# OVERLAPS #-} HasServer (Get String) where
  route ∷ Proxy (Get String) → HandlerAction String → Request → Maybe (HandlerAction String)
  route _ handlerAction [] = Just handlerAction
  route _ _ _ = Nothing

instance (HasServer a, HasServer b) ⇒ HasServer (a :<|> b) where
  route ∷ Proxy (a :<|> b) → (Server a :<|> Server b) → Request → Maybe (HandlerAction String)
  route _ (srvReqA :<|> srvReqB) req = route (Proxy ∷ Proxy a) srvReqA req <|> route (Proxy ∷ Proxy b) srvReqB req

instance (KnownSymbol s, HasServer r) ⇒ HasServer ((s ∷ Symbol) :> r) where
  route ∷ Proxy (s :> r) → Server r → Request → Maybe (HandlerAction String)
  route _ srvReq (x : xs)
    | symbolVal (Proxy ∷ Proxy s) == x = route (Proxy ∷ Proxy r) srvReq xs
  route _ _ _ = Nothing

instance (Read a, HasServer r) ⇒ HasServer (Capture a :> r) where
  route ∷ Proxy (Capture a :> r) → (a → Server r) → Request → Maybe (HandlerAction String)
  route _ handler (x : xs) = do
    a ← readMaybe x
    route (Proxy ∷ Proxy r) (handler a) xs
  route _ _ _ = Nothing

--------------------------------------------------------------------------------------------------------------------

-- `get` is a small wrapper for `route`
get ∷ HasServer api ⇒ Proxy api → Server api → Request → IO String
get proxy srvReq request = fromMaybe (pure "Malformed request") (route proxy srvReq request)

------------------------
-- 1st implementation --
------------------------

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 []
-- "Ok"
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["title", "4711"]
-- "Haskell in Depth"
--
-- This is what you get if you don't have the overlapping instance for String:
-- get (Proxy ∷ Proxy BookInfoAPI) impl1 ["title", "4711"]
-- "\"Haskell in Depth\""
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["year", "4711"]
-- "2021"
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl1 ["rating", "4711"]
-- "Great"
--
------------------------
-- 2nd implementation --
------------------------

-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 []
-- "Down"
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["title", "4711"]
-- user error (not implemented)
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["year", "4711"]
-- user error (not implemented)
--
-- >>> get (Proxy ∷ Proxy BookInfoAPI) impl2 ["rating", "4711"]
-- user error (not implemented)

check ∷ Server BookInfoAPI → IO ()
check impl = do
  b ← get (Proxy ∷ Proxy BookInfoAPI) impl []
  answer ← get (Proxy ∷ Proxy BookInfoAPI) impl ["year", "4711"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = do
  check impl1 -- "OK"
  check impl2 -- "api-stage3: user error (not implemented)"
