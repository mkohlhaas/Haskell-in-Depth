{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}

import Control.Monad.State
import Data.Coerce

------------------------------
-- Abstrace Data Type (ADT) --
------------------------------

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- safely convert between values of types that have the same representation with no run-time overhead
-- >>> :t coerce
-- coerce ∷ Coercible a b ⇒ a → b

-- Looking at the generated code it turns out implementing any method is as easy as writing `coerce`!
--
-- uses Int's (==)
-- instance Eq Age where
--   (==)
--     = coerce
--         @(Int → Int → Bool)
--         @(Age → Age → Bool)
--         ((==) @Int) ∷ Age → Age → Bool
--
-- uses Int's `compare`
-- instance Ord Age where
--   compare
--     = coerce
--         @(Int → Int → Ordering)
--         @(Age → Age → Ordering)
--         (compare @Int) ∷ Age → Age → Ordering

-- >>> Age 42 == Age 42
-- True

-- >>> Age 42 < Age 42
-- False

-- >>> Age 42 < Age 43
-- True

-- >>> Age 43 < Age 42
-- False

------------------
-- Monad Stacks --
------------------

newtype MyApp a = MyApp {runApp ∷ StateT Int IO a}
  deriving (Functor, Applicative, Monad)

-- MyApp's fmap uses IO's fmap.
-- instance Functor MyApp where
--   fmap
--     = coerce
--         @((a → b) → IO a → IO b)
--         @((a → b) → MyApp a → MyApp b)
--         (fmap @(IO)) ∷ ∀ a b. (a → b) → MyApp a → MyApp b

-- MyApp's pure uses StateT's pure.
-- instance Applicative MyApp where
--   pure
--     = coerce
--         @(a → StateT Int IO a)
--         @(a → MyApp a)
--         (pure @(StateT Int IO)) ∷ ∀ a. a → MyApp a

-- MyApp's bind uses StateT's bind.
-- instance Monad MyApp where
--   (>>=)
--     = coerce
--         @(StateT Int IO a → (a → StateT Int IO b) → StateT Int IO b)
--         @(MyApp a → (a → MyApp b) → MyApp b)
--         ((>>=) @(StateT Int IO)) ∷ ∀ a b. MyApp a → (a → MyApp b) → MyApp b

-------------------
-- Type Families --
-------------------

type family Inspect t

type instance Inspect Int = Bool

type instance Inspect Age = Int

class Inspector a where
  inspect ∷ a → Inspect a

instance Inspector Int where
  inspect ∷ Int → Inspect Int -- ⇒ Int → Bool
  inspect n = n > 0

-- GHC had to implement this:
-- Compiler error: Couldn't match type ‘Bool’ with ‘Int’
-- instance Inspector Age where
--   inspect ∷ Age → Inspect Age -- ⇒ Age → Int
--   inspect n = n > 0

-- Doesn't compile:
-- GHC compiler error: "Couldn't match representation of type ‘Bool’ with that of ‘Int’ arising from a use of ‘coerce’."
-- deriving newtype instance Inspector Age -- using the StandaloneDeriving extension

main ∷ IO ()
main = print $ Age 42 < Age 42 -- False
