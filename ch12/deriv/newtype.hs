{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Control.Monad.State
import Data.Coerce

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- GCH generates this:
-- instance Eq Age where
--   (==) = coerce ((==) ∷ Int → Int → Bool)

-- GCH generates this:
-- instance Ord Age where
--   compare = coerce (compare ∷ Int → Int → Ordering)

newtype MyApp a = MyApp {runApp :: StateT Int IO a}
  deriving (Functor, Applicative, Monad)

-- GCH generates this:
-- instance Functor MyApp where
--   fmap ∷ ∀ a b. (a → b) → MyApp a → MyApp b
--   fmap = coerce (fmap ∷ (a → b) → StateT Int IO a → StateT Int IO b)

type family Inspect t

type instance Inspect Int = Bool

type instance Inspect Age = Int

class Inspector a where
  inspect :: a -> Inspect a

instance Inspector Int where
  inspect n = n > 0

-- Is it possible to derive the Inspector instance for Age using the newtype strategy?
-- Remember that the newtype strategy here is replacing an implementation for Age with an implementation for Int.
-- An ability to do that would mean that we have a result of type Bool (Inspect Int) interpreted as a value of type Int (Inspect.Age).
-- By doing so, we'd break the Haskell type system.

-- doesn't compile:
-- deriving newtype instance Inspector Age

-- |
-- >>> Age 42 < Age 42
-- False
main :: IO ()
main = do
  print $ Age 42 < Age 42 -- False
