{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -ddump-deriv #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- {-# LANGUAGE DerivingStrategies #-}

import Data.Monoid -- (Alt (Alt), Dual (Dual))

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- GHC uses `coerce`:
-- instance GHC.Classes.Eq Main.Age where
--   (GHC.Classes.==)
--     = GHC.Prim.coerce
--         @(GHC.Types.Int → GHC.Types.Int → GHC.Types.Bool)
--         @(Main.Age → Main.Age → GHC.Types.Bool)
--         ((GHC.Classes.==) @GHC.Types.Int) ∷ Main.Age → Main.Age → GHC.Types.Bool

-- >>> Age 42 == Age 42
-- True

-- We give GHC an example of a type that already implements the instance we need and say:
-- "Generate an implementation in the same way but replace types with ours!"
newtype Age' = Age' Int
  deriving (Eq, Ord) via Int

-- same as `newtype` deriving:
-- instance GHC.Classes.Eq Main.Age' where
--   (GHC.Classes.==)
--     = GHC.Prim.coerce
--         @(GHC.Types.Int → GHC.Types.Int → GHC.Types.Bool)
--         @(Main.Age' → Main.Age' → GHC.Types.Bool)
--         ((GHC.Classes.==) @GHC.Types.Int) ∷ Main.Age' → Main.Age' → GHC.Types.Bool

-- >>> Age' 42 == Age' 42
-- True

-- `Alt` prefers the first available not-Nothing value.
-- `Dual` prefers the last available not-Nothing value by swapping arguments to `<>`.

-- >>> :type mappend
-- mappend ∷ Monoid a ⇒ a → a → a

-- >>> "Hello" <> "World"
-- "HelloWorld"

-- From Pursuit: The dual of a monoid.
-- Dual x <> Dual y == Dual (y <> x)
-- (mempty ∷ Dual _) == Dual mempty

-- swapping the arguments of mappend (<>)
-- >>> Dual "Hello" <> Dual "World"
-- Dual {getDual = "WorldHello"}

-- >>> :type getDual
-- getDual ∷ Dual a → a

-- swapping the arguments of mappend (<>)
-- >>> getDual $ Dual "Hello" <> Dual "World"
-- "WorldHello"

-- Doesn't work: There are no Semigroup and Monoid instances for Int.
-- GHC compiler error: "Could not deduce (Semigroup Int)."
-- newtype MAge = MAge (Maybe Int)
--   deriving newtype (Semigroup, Monoid)

-- With the DerivingVia extension we can instruct GHC to follow the `Alt Maybe Int` example.
-- Alt and Dual do have Semigroup and Monoid instances.
newtype MAge = MAge (Maybe Int)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Alt Maybe Int) ------------ take the first available value
  -- deriving (Semigroup, Monoid) via (Dual (Alt Maybe Int)) -- take the last  available value

-- Alternatively we could use Standalone deriving:
-- deriving via (Alt Maybe Int) instance Semigroup MAge
-- deriving via (Alt Maybe Int) instance Monoid MAge

-- Is `MAge (Just 42)` or `MAge (Just 24)` depending on Alt or `Dual Alt`.
-- >>> MAge (Just 42) <> MAge (Just 24)
-- MAge (Just 42)

main ∷ IO ()
main = print $ Just (MAge (Just 42)) <> Just (MAge (Just 24))
