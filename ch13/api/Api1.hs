{-# LANGUAGE GADTs #-}

import Text.Read (readMaybe)

type BookID = Int

type HandlerAction = IO

data ServiceStatus = Ok | Down
  deriving (Show)

data Rating = Bad | Good | Great
  deriving (Show)

type ReqHandler a = BookID → HandlerAction a

type Request = [String]

-- data BookInfoAPIImpl = BookInfoAPIImpl
--   { root ∷ HandlerAction ServiceStatus,
--     title ∷ ReqHandler String,
--     year ∷ ReqHandler Int,
--     rating ∷ ReqHandler Rating
--   }

-- in GADT
data BookInfoAPIImpl where
  BookInfoAPIImpl ∷
    { root ∷ HandlerAction ServiceStatus,
      title ∷ ReqHandler String,
      year ∷ ReqHandler Int,
      rating ∷ ReqHandler Rating
    } →
    BookInfoAPIImpl

impl1 ∷ BookInfoAPIImpl
impl1 =
  BookInfoAPIImpl
    { root = pure Ok,
      title = const $ pure "Haskell in Depth", -- const as we don't care about BookID
      year = const $ pure 2021,
      rating = const $ pure Great
    }

impl2 ∷ BookInfoAPIImpl
impl2 =
  BookInfoAPIImpl
    { root = pure Down,
      title = const notImplemented,
      year = const notImplemented,
      rating = const notImplemented
    }
  where
    notImplemented = fail "not implemented"

encode ∷ Show a ⇒ HandlerAction a → HandlerAction String
encode m = show <$> m

route ∷ BookInfoAPIImpl → Request → Maybe (HandlerAction String)
route impl [] = pure $ encode $ root impl
route impl [op, bookId'] = do
  bookId ← readMaybe bookId'
  case op of
    "title" → pure $ title impl bookId
    "year" → pure $ encode $ year impl bookId
    "rating" → pure $ encode $ rating impl bookId
    _ → Nothing
route _ _ = Nothing

get ∷ BookInfoAPIImpl → Request → HandlerAction String
get impl req =
  case route impl req of
    Nothing → pure "Malformed request"
    Just m → m

-- >>> get impl1 ["title", "7548"]
-- "Haskell in Depth"

-- >>> get impl1 ["year", "7548"]
-- "2021"

-- >>> get impl1 ["rating", "7548"]
-- "Great"

-- >>> get impl1 ["useless", "7548"]
-- "Malformed request"

-- >>> get impl2 ["title", "7548"]
-- user error (not implemented)

-- >>> get impl2 ["year", "7548"]
-- user error (not implemented)

-- >>> get impl2 ["rating", "7548"]
-- user error (not implemented)

-- >>> get impl2 ["useless", "7548"]
-- "Malformed request"

check ∷ BookInfoAPIImpl → IO ()
check impl = do
  b ← get impl []
  answer ← get impl ["year", "7548"]
  putStrLn (if b == "Ok" && answer == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = check impl1
