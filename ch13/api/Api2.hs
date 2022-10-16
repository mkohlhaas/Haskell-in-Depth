{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Text.Read (readMaybe)

data Rating = Bad | Good | Great
  deriving (Show)

data ServiceStatus = Ok | Down
  deriving (Show)

-- description of the web interface
data Get (a ∷ Type)  -- describes the result

data Capture (a ∷ Type) -- captures request parameter

data a :<|> b = a :<|> b -- lists alternative request operations

infixr 8 :<|>

data (a ∷ k) :> (b ∷ Type) -- adds a request component

infixr 9 :>
-- end of description of the web interface

type BookID = Int

type BookInfoAPI =
  Get ServiceStatus
    :<|> "title" :> Capture BookID :> Get String
    :<|> "year" :> Capture BookID :> Get Int
    :<|> "rating" :> Capture BookID :> Get Rating

type HandlerAction a = IO a

-- This type is not used anywhere
-- BookInfoAPIImpl is the same as 'Server BookInfoAPI'
type BookInfoAPIImpl =
  HandlerAction ServiceStatus
    :<|> (BookID → HandlerAction String)
    :<|> (BookID → HandlerAction Int)
    :<|> (BookID → HandlerAction Rating)

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

type Request = [String]

encode ∷ Show a ⇒ IO a → IO String
encode m = show <$> m

route ∷ Server BookInfoAPI → Request → Maybe (IO String)
route (root :<|> _) [] = pure $ encode root
route (_ :<|> title :<|> year :<|> rating) [op, bid'] = do
  bid ← readMaybe bid'
  case op of
    "title" → pure $ title bid
    "year" → pure $ encode $ year bid
    "rating" → pure $ encode $ rating bid
    _ → Nothing
route _ _ = Nothing

get ∷ Server BookInfoAPI → Request → IO String
get impl xs =
  case route impl xs of
    Just m → m
    Nothing → pure "Malformed request"

check ∷ Server BookInfoAPI → IO ()
check impl = do
  b ← get impl []
  answer ← get impl ["year", "7548"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = check impl1
