get ∷ [String] → IO String
get [] = pure "OK"
get [op, _] =
  case op of
    "title" → pure "Haskell in Depth"
    "year" → pure "2021"
    "rating" → pure "Great"
    _ → fail "Not implemented"
get _ = fail "Malformed request"

-- >>> get ["title", "7548"]
-- "Haskell in Depth"

-- >>> get ["foo", "7548"]
-- user error (Not implemented)

-- >>> get ["foo", "7548", "useless"]
-- user error (Malformed request)

-- >>> get ["year", "7548"]
-- "2021"

-- >>> get ["rating", "7548"]
-- "Great"

check ∷ IO ()
check = do
  b ← get []
  y ← get ["year", "7548"]
  putStrLn (if b == "OK" && y == "2021" then "OK" else "Wrong answer!")

main ∷ IO ()
main = check
