{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Rating = Bad | Good | Great
  deriving (Show)

data ServiceStatus = Ok | Down
  deriving (Show)

-------------------------------------------------
--      Description of the Web Interface       --
-------------------------------------------------

-- describes the result (output)
data Get a

-- captures request parameter (input)
data Capture a

-- alternative requests
data a :<|> b = a :<|> b

infixr 8 :<|>

-- no data constructor as it will be eliminated by a type family
-- adds a request component
data a :> b

-- has a higher precedence than (:<|>)
infixr 9 :>

---------------------------------------------
-- End of Description of the Web Interface --
---------------------------------------------

type BookID = Int

-- Symbols "title", "year" and "rating" not necessary but clarifies usage of this type.
-- Acts like a documentation string.
type BookInfoAPI =
  Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String
    :<|> "year" :> Capture BookID :> Get Int
    :<|> "rating" :> Capture BookID :> Get Rating

type HandlerAction = IO

------------------------
-- Server Type Family --
------------------------

type family ServerReq api

-- no transformation for Capture
-- >>> :kind! Server (Capture BookID)
-- Server (Capture BookID) ∷ Type
-- = Server (Capture BookID)

-- wraps the result into the IO monad
-- Get is always at the end
type instance ServerReq (Get a) = HandlerAction a

-- >>> :kind! Server (Get Int)
-- Server (Get Int) ∷ Type
-- = HandlerAction Int

-- input param turns into functions
-- and recurse on RHS
type instance ServerReq (Capture a :> r) = a → ServerReq r

-- >>> :kind! Server (Capture BookID :> Get Int)
-- Server (Capture BookID :> Get Int) ∷ Type
-- = Int → HandlerAction Int

-- >>> :kind! Server (Capture BookID :> Capture Int :> Get Bool)
-- Server (Capture BookID :> Capture Int :> Get Bool) ∷ Type
-- = Int → Int → HandlerAction Bool

-- discard Symbols like "title", "year" and "rating"
-- and recurse on RHS
type instance ServerReq ((s ∷ Symbol) :> r) = ServerReq r

-- >>> :kind! Server ("title" :> (Capture BookID))
-- Server ("title" :> (Capture BookID)) :: Type
-- = Server (Capture BookID)

-- recurse on both alternatives
type instance ServerReq (a :<|> b) = ServerReq a :<|> ServerReq b

---------------------
-- Implementations --
---------------------

-- >>> :kind! Server BookInfoAPI
-- Server BookInfoAPI ∷ Type
-- = HandlerAction ServiceStatus ---------------------- root
--   :<|> ((Int → HandlerAction String) --------------- title
--   :<|> ((Int → HandlerAction Int) ------------------ year
--   :<|> (Int → HandlerAction Rating))) -------------- rating

impl1 ∷ ServerReq BookInfoAPI
impl1 =
  pure Ok
    :<|> title
    :<|> year
    :<|> rating
  where
    title _ = pure "Haskell in Depth"
    year _ = pure 2021
    rating _ = pure Great

impl2 ∷ ServerReq BookInfoAPI
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

type Request = [String]

encode ∷ Show a ⇒ IO a → IO String
encode a = show <$> a

route ∷ ServerReq BookInfoAPI → Request → Maybe (HandlerAction String)
route (root :<|> _) [] = pure $ encode root
route (_ :<|> title :<|> year :<|> rating) [op, bookId'] = do
  bookId ← readMaybe bookId'
  case op of
    "title" → pure $ title bookId
    "year" → pure $ encode $ year bookId
    "rating" → pure $ encode $ rating bookId
    _ → Nothing
route _ _ = Nothing

-- `get` is a small wrapper for `route`
get ∷ ServerReq BookInfoAPI → Request → HandlerAction String
get impl request = fromMaybe (pure "Malformed request") (route impl request)

------------------------
-- 1st implementation --
------------------------

-- >>> get impl1 []
-- "Ok"
--
-- >>> get impl1 ["title", "4711"]
-- "Haskell in Depth"
--
-- >>> get impl1 ["year", "4711"]
-- "2021"
--
-- >>> get impl1 ["rating", "4711"]
-- "Great"

------------------------
-- 2nd implementation --
------------------------

-- >>> get impl2 []
-- "Down"
--
-- >>> get impl2 ["title", "4711"]
-- user error (not implemented)
--
-- >>> get impl2 ["year", "4711"]
-- user error (not implemented)
--
-- >>> get impl2 ["rating", "4711"]
-- user error (not implemented)

check ∷ ServerReq BookInfoAPI → IO ()
check impl = do
  b ← get impl []
  answer ← get impl ["year", "4711"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = do
  check impl1 -- "OK"
  check impl2 -- "api-stage2: user error (not implemented)"
