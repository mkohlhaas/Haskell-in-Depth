{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving (Show)

data ServiceStatus = Ok | Down
  deriving (Show)

-------------------------------------------------
--      Description of the Web Interface       --
-------------------------------------------------

-- describes the result (output)
-- data Get (a ∷ Type)
data Get a

-- >>> :kind Get
-- Get ∷ k → Type

-- captures request parameter (input)
-- data Capture (a ∷ Type)
data Capture a

-- >>> :kind! Capture
-- Capture ∷ k → Type
-- = Capture

-- multiple/alternative requests
data a :<|> b = a :<|> b

infixr 8 :<|>

-- adds a request component
-- data (a ∷ k) :> (b ∷ Type)
data a :> b

-- >>> :kind! (:>)
-- (:>) ∷ k1 → k2 → Type
-- = (:>)

-- >>> :kind "hello" :> "world"
-- "hello" :> "world" ∷ Type

-- >>> :kind "hello" :> Capture Int
-- "hello" :> Capture Int ∷ Type

-- has a higher precedence than (:<|>)
infixr 9 :>

-- >>> :type "hello"
-- "hello" ∷ [Char]

-- >>> :kind "hello"
-- "hello" ∷ Symbol

-------------------------------------------------
-- End of the Description of the Web Interface --
-------------------------------------------------

type BookID = Int

-- Symbols "title", "year" and "rating" not necessary but clarifies usage of this type.
-- It's like a documentation string.
type BookInfoAPI =
  Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String
    :<|> "year" :> Capture BookID :> Get Int
    :<|> "rating" :> Capture BookID :> Get Rating

-- the same
-- type BookInfoAPI =
--   Get ServiceStatus
--     :<|> Capture BookID :> Get String -- title
--     :<|> Capture BookID :> Get Int ----- year
--     :<|> Capture BookID :> Get Rating -- rating

type HandlerAction = IO

-- Means basically the same as BookInfoAPI.
-- (This type is unused.)
type BookInfoAPIImpl =
  HandlerAction ServiceStatus
    :<|> (BookID → HandlerAction String)
    :<|> (BookID → HandlerAction Int)
    :<|> (BookID → HandlerAction Rating)

type family Server api ∷ Type

-- wraps the result into the IO monad
type instance Server (Get a) = HandlerAction a

-- (:<|>) acts as a sum type
type instance Server (a :<|> b) = Server a :<|> Server b

-- ignore Symbols like "title", "year" and "rating".
type instance Server ((s ∷ Symbol) :> r) = Server r

-- input params turns into a function
type instance Server (Capture a :> r) = a → Server r

-- >>> :kind! Server (Get Int)
-- Server (Get Int) ∷ Type
-- = HandlerAction Int

-- >>> :kind! Server ("title" :> (Capture BookID))
-- Server ("title" :> (Capture BookID)) ∷ Type
-- = Server (Capture BookID)

-- >>> :kind! Server (Capture BookID)
-- Server (Capture BookID) ∷ Type
-- = Server (Capture BookID)

-- >>> :kind! Server (Capture BookID :> Get Int)
-- Server (Capture BookID :> Get Int) ∷ Type
-- = Int → HandlerAction Int

-- >>> :kind! Server (Capture BookID :> Capture Int :> Get Bool)
-- Server (Capture BookID :> Capture Int :> Get Bool) ∷ Type
-- = Int → Int → HandlerAction Bool

-- >>> :kind! (Server (Capture BookID :> Capture BookID :> Get Bool)) :<|> (Server (Capture BookID :> Capture Bool :> Get BookID))
-- (Server (Capture BookID :> Capture BookID :> Get Bool)) :<|> (Server (Capture BookID :> Capture Bool :> Get BookID)) ∷ Type
-- = (Int → Int → HandlerAction Bool) :<|> (Int → Bool → HandlerAction Int)

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

type Request = [String]

encode ∷ Show a ⇒ IO a → IO String
encode a = show <$> a

route ∷ Server BookInfoAPI → Request → Maybe (HandlerAction String)
route (root :<|> _) [] = pure $ encode root
route (_ :<|> title :<|> year :<|> rating) [op, bookId'] = do
  bookId ← readMaybe bookId'
  case op of
    "title" → pure $ title bookId
    "year" → pure $ encode $ year bookId
    "rating" → pure $ encode $ rating bookId
    _ → Nothing
route _ _ = Nothing

get ∷ Server BookInfoAPI → Request → HandlerAction String
get impl xs =
  case route impl xs of
    Just m → m
    Nothing → pure "Malformed request"

-- >>> get impl1 []
-- "Ok"

-- >>> get impl1 ["title", "4711"]
-- "Haskell in Depth"

-- >>> get impl1 ["year", "4711"]
-- "2021"

-- >>> get impl1 ["rating", "4711"]
-- "Great"

-- >>> get impl2 []
-- "Down"

-- >>> get impl2 ["title", "4711"]
-- user error (not implemented)

-- >>> get impl2 ["year", "4711"]
-- user error (not implemented)

-- >>> get impl2 ["rating", "4711"]
-- user error (not implemented)

check ∷ Server BookInfoAPI → IO ()
check impl = do
  b ← get impl []
  answer ← get impl ["year", "4711"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = check impl1
