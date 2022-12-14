{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -ddump-deriv #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- {-# LANGUAGE DerivingStrategies #-}

import Data.Monoid -- (Alt (Alt), Dual (Dual))

----------
-- ADTs --
----------

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- Age's (==) uses Int's (==).
-- instance Eq Age where
--   (==)
--     = coerce
--         @(Int → Int → Bool)
--         @(Age → Age → Bool)
--         ((==) @Int) ∷ Age → Age → Bool

-- >>> Age 42 == Age 42
-- True

-- We give GHC an example of a type that already implements the instance we need and say:
-- "Generate an implementation in the same way but replace with ours types!"
newtype Age' = Age' Int
  deriving (Eq, Ord) via Int

-- same as `newtype` deriving:
-- instance Eq Age' where
--   (==)
--     = coerce
--         @(Int → Int → Bool)
--         @(Age' → Age' → Bool)
--         ((==) @Int) ∷ Age' → Age' → Bool

-- >>> Age' 42 == Age' 42
-- True

------------------------
-- Alt/Dual Explained --
------------------------

-- `Alt` prefers the first available not-Nothing value.
-- `Dual` prefers the last available not-Nothing value by swapping arguments to `<>`.

-- >>> :type Alt
-- Alt ∷ f a → Alt f a

-- >>> :type Dual
-- Dual ∷ a → Dual a

-- >>> :info Alt
-- newtype Alt f a = Alt {getAlt ∷ f a}

-- >>> :info Dual
-- newtype Dual a = Dual {getDual ∷ a}

-- >>> :type mappend
-- mappend ∷ Monoid a ⇒ a → a → a

-- >>> "Hello" <> "World"
-- "HelloWorld"

-- >>> getAlt (Alt (Just 12) <> Alt (Just 24))
-- Just 12

-- >>> getAlt $ Alt Nothing <> Alt (Just 24)
-- Just 24

-- >>> Dual (Alt (Just 12)) <> Dual (Alt (Just 24))
-- Dual {getDual = Alt {getAlt = Just 24}}

-- >>> Dual (Alt Nothing) <> Dual (Alt (Just 24))
-- Dual {getDual = Alt {getAlt = Just 24}}

-- From Pursuit: The dual of a monoid.
-- Dual x <> Dual y == Dual (y <> x)
-- (mempty ∷ Dual _) == Dual mempty

-- swapping the arguments of mappend (<>)
-- >>> getDual $ Dual "Hello" <> Dual "World"
-- "WorldHello"

-----------------------------------
-- Deriving Semigroup and Monoid --
-----------------------------------

-- GHC compiler error: "Could not deduce (Semigroup Int)."
-- There are no Semigroup and Monoid instances for Int.
-- newtype MAge = MAge (Maybe Int)
--   deriving newtype (Semigroup, Monoid)

-- With the DerivingVia extension we can instruct GHC to follow the `Alt Maybe Int` example.
-- Alt and Dual do have Semigroup and Monoid instances.
newtype MAge = MAge (Maybe Int)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Alt Maybe Int) ------------ take the first available value
  -- deriving (Semigroup, Monoid) via (Dual (Alt Maybe Int)) -- take the last  available value

-- Alternatively we could use Standalone deriving.
-- Note the syntax!
-- deriving via (Alt Maybe Int) instance Semigroup MAge
-- deriving via (Alt Maybe Int) instance Monoid MAge

-- >>> MAge (Just 42) <> MAge (Just 24)
-- MAge (Just 42)

main ∷ IO ()
main = print $ MAge (Just 42) <> MAge (Just 24)
