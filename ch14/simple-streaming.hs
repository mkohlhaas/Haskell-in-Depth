{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad (ap)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Functor.Compose (Compose (Compose))

-- f = functor ⇒ Stream will become a monad
-- There can only one Return in a Stream.
data Stream f m r
  = Return !r
  | Effect !(m (Stream f m r))
  | Step !(f (Stream f m r))

empty ∷ Stream f m ()
empty = Return ()

-- m r = monadic result
effect ∷ Monad m ⇒ m r → Stream f m r
effect eff = Effect $ Return <$> eff

-- Generalization to functors is done in Haskell very often.
-- Stream is actually called a FREE MONAD.
instance (Functor f, Monad m) ⇒ Functor (Stream f m) where
  fmap ∷ ∀ a b. (a → b) → Stream f m a → Stream f m b
  fmap fn (Return r) = Return $ fn r
  fmap fn (Effect ma) = Effect $ fmap fn <$> ma
  fmap fn (Step f) = Step $ fmap fn <$> f

-- A data type for a free monad has following structure:
-- data Free f a = Pure a | Free (f (Free f a))

-- Another traditional thing is to check whether our type is a monad.
instance (Functor f, Monad m) ⇒ Monad (Stream f m) where
  (>>=) ∷ ∀ r r1. Stream f m r → (r → Stream f m r1) → Stream f m r1
  stream >>= fn = loop stream
    where
      loop ∷ Stream f m r → Stream f m r1
      loop (Return r) = fn r
      loop (Effect m) = Effect $ loop <$> m
      loop (Step f) = Step $ loop <$> f

instance (Functor f, Monad m) ⇒ Applicative (Stream f m) where
  pure = Return
  (<*>) = ap

-- acts like a pair
data Of a b = a :> b
  deriving (Show)

instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

yield ∷ a → Stream (Of a) m ()
yield a = Step (a :> empty)

each ∷ [e] → Stream (Of e) m ()
each [] = Return ()
each (x : xs) = Step $ x :> each xs

ssum ∷ (Num e, Monad m) ⇒ Stream (Of e) m r → m (Of e r)
ssum (Return r) = pure (0 :> r)
ssum (Effect ma) = ma >>= ssum
ssum (Step (e :> str)) = first (+ e) <$> ssum str

printStream ∷ (Show e, Show r) ⇒ Stream (Of e) IO r → IO ()
printStream (Return r) = putStrLn $ "Result: " <> show r
printStream (Effect m) = do
  putStrLn "Run action:"
  stream ← m
  printStream stream
printStream (Step (e :> stream)) = do
  putStrLn $ "Element: " <> show e
  printStream stream

stream1 ∷ Stream (Of Int) IO ()
stream1 = do
  yield 1
  effect (putStrLn "action 1")
  yield 2
  effect (putStrLn "action 2")
  yield 3

-- replacing the functor on Steps
-- maps ∷ (Functor m, Functor f) ⇒ (f (Stream g m r) → g (Stream g m r)) → Stream f m r → Stream g m r
maps ∷ (Functor f, Monad m) ⇒ (∀ x. f x → g x) → Stream f m r → Stream g m r
maps fn = loop
  where
    -- loop ∷ Stream f m r → Stream g m r
    loop (Return r) = Return r
    loop (Effect m) = Effect (loop <$> m)
    loop (Step f) = Step (fn (loop <$> f))

mapOf ∷ Monad m ⇒ (a → b) → Stream (Of a) m r → Stream (Of b) m r
mapOf fn = maps (first fn)

-- >>> ssum $ mapOf (* 2) $ each [1..3 ∷ Int]
-- 12 :> ()

-- zipsWith ∷ Functor m ⇒ ((Stream f m r → Stream g m r → Stream h m r) → f (Stream f m r) → g (Stream g m r) → h (Stream h m r)) → Stream f m r → Stream g m r → Stream h m r
zipsWith ∷ Monad m ⇒ (∀ x y p. (x → y → p) → f x → g y → h p) → Stream f m r → Stream g m r → Stream h m r
zipsWith fn = loop
  where
    -- loop ∷ Stream f m r → Stream g m r → Stream h m r
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect m) t = Effect $ (`loop` t) <$> m
    loop s (Effect n) = Effect $ loop s <$> n
    loop (Step fs) (Step gs) = Step $ fn loop fs gs

zipPair ∷ Monad m ⇒ Stream (Of a) m r → Stream (Of b) m r → Stream (Of (a, b)) m r
zipPair = zipsWith fn
  where
    -- fn ∷ (t → t1 → b1) → Of a t → Of b t1 → Of (a, b) b1
    fn p (a :> x) (b :> y) = (a, b) :> p x y

compose ∷ (Monad m, Functor f, Functor g) ⇒ Stream f m r → Stream g m r → Stream (Compose f g) m r
compose = zipsWith fn
  where
    -- fn ∷ (t → a → a1) → f t → g a → Compose f g a1
    fn p ft ga = Compose $ (\x → p x <$> ga) <$> ft

decompose ∷ (Monad m, Functor f) ⇒ Stream (Compose m f) m r → Stream f m r
decompose = loop
  where
    -- loop ∷ Stream (Compose m f) m r → Stream f m r
    loop (Return r) = Return r
    loop (Effect m) = Effect (loop <$> m)
    loop (Step (Compose mstr)) = Effect $ do Step . fmap loop <$> mstr

--                                 |          |
--                 outer functor becomes an Effect
--                                            |
--                                  inner functor remains a Step

-- inserting a monadic effect into every layer of a stream
-- mapsM ∷ (Monad m, Functor g, Functor f) ⇒ (f (Stream (Compose m g) m r) → m (g (Stream (Compose m g) m r))) → Stream f m r → Stream g m r
mapsM ∷ (Monad m, Functor f, Functor g) ⇒ (∀ x. f x → m (g x)) → Stream f m r → Stream g m r
mapsM fn = decompose . maps (Compose . fn)

-- adding any monadic effect to a `Stream (Of a)` without hurting the stream elements
withEffect ∷ Monad m ⇒ (a → m ()) → Stream (Of a) m r → Stream (Of a) m r
withEffect eff = mapsM go
  where
    -- go ∷ Of a b → m (Of a b)
    go p@(e :> _) = eff e >> pure p

withPrinting ∷ Show a ⇒ Stream (Of a) IO r → Stream (Of a) IO r
withPrinting = withEffect (\e → putStrLn ("Element: " ++ show e))

-- creates a Stream of Streams
-- splits at Steps
splitsAt ∷ (Monad m, Functor f) ⇒ Int → Stream f m r → Stream f m (Stream f m r)
splitsAt = loop
  where
    loop n stream
      | n > 0 =
        case stream of
          Return r → Return (Return r) -- stream has ended
          Effect m → Effect (loop n <$> m)
          Step f → Step (loop (n -1) <$> f)
      | otherwise = Return stream

-- every Step turns into a Stream
chunksOf ∷ ∀ f m r. (Monad m, Functor f) ⇒ Int → Stream f m r → Stream (Stream f m) m r
chunksOf n = loop
  where
    cutChunk ∷ Stream f m r → Stream f m (Stream (Stream f m) m r)
    cutChunk stream = loop <$> splitsAt (n -1) stream
    loop ∷ Stream f m r → Stream (Stream f m) m r
    loop (Return r) = Return r
    loop (Effect m) = Effect (loop <$> m)
    loop (Step fs) = Step (Step (cutChunk <$> fs))

-- >>> ssum $ mapsM ssum $ chunksOf 2 $ each [1,1,1,1,1 ∷ Int]
-- 5 :> ()

main ∷ IO ()
main = do
  s :> () ← ssum $ withEffect print $ mapsM ssum $ chunksOf 2 $ each [1, 1, 1, 1, 1 ∷ Int]
  print s -- 2 2 1 5
