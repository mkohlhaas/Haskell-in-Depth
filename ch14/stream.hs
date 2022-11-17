-- e = element, m = monadic action, r = result
-- monadic action generates a stream
data Stream e m r
  = Element e (Stream e m r)
  | Action (m (Stream e m r)) -- creates a stream in Monad m
  | Result r

empty ∷ Stream e m ()
empty = Result ()

stream ∷ Stream Int IO ()
stream = Element 1 (Action (putStrLn "some action" >> pure (Element 2 (Action (putStrLn "finish" >> pure empty)))))

-- Stream in the IO monad
printStream ∷ (Show e, Show r) ⇒ Stream e IO r → IO ()
printStream (Element e stream) = do
  putStrLn $ "Element: " <> show e
  printStream stream
printStream (Action iostream) = do
  putStr "Run action: "
  stream ← iostream
  printStream stream
printStream (Result r) = putStrLn $ "Result: " <> show r

-- sum all elements
ssum ∷ (Num e, Monad m) ⇒ Stream e m r → m (e, r)
ssum (Element e stream) = (\(acc, r) → (acc + e, r)) <$> ssum stream
ssum (Action ma) = ma >>= ssum -- ma = monadic action
ssum (Result r) = pure (0, r)

-- GHCi is in the IO monad and prints result
-- >>> ssum stream
-- (3,())

-- only Elements, no Actions
each ∷ [e] → Stream e m ()
each = foldr Element (Result ())

-- >>> ssum $ each [1 .. 10 ∷ Int]
-- (55,())

main ∷ IO ()
main = do
  printStream stream
  putStrLn "-------------------------"
  ssum stream >>= print
  putStrLn "-------------------------"
  ssum (each [1 .. 10 ∷ Int]) >>= print
