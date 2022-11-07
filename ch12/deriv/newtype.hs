{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-deriv #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

import Control.Monad.State
import Data.Coerce

newtype Age = Age Int
  deriving newtype (Eq, Ord)

-- It turns out implementing any method is as easy as writing `coerce`!!!

-- GHC basically generates this:
-- instance Eq Age where
--   (==) = coerce ((==) ∷ Int → Int → Bool)

-- instance GHC.Classes.Eq Main.Age where
--   (GHC.Classes.==)
--     = GHC.Prim.coerce
--         @(GHC.Types.Int → GHC.Types.Int → GHC.Types.Bool)
--         @(Main.Age → Main.Age → GHC.Types.Bool)
--         ((GHC.Classes.==) @GHC.Types.Int) ∷ Main.Age → Main.Age → GHC.Types.Bool

-- GHC basically generates this:
-- instance Ord Age where
--   compare = coerce (compare ∷ Int → Int → Ordering)

-- instance GHC.Classes.Ord Main.Age where
--   GHC.Classes.compare
--     = GHC.Prim.coerce
--         @(GHC.Types.Int → GHC.Types.Int → GHC.Types.Ordering)
--         @(Main.Age → Main.Age → GHC.Types.Ordering)
--         (GHC.Classes.compare @GHC.Types.Int) ∷ Main.Age → Main.Age → GHC.Types.Ordering

-- >>> Age 42 == Age 42
-- True

-- >>> Age 42 < Age 42
-- False

-- >>> Age 42 < Age 43
-- True

-- >>> Age 43 < Age 42
-- False

newtype MyApp a = MyApp {runApp ∷ StateT Int IO a}
  deriving (Functor, Applicative, Monad)

-- >>> :type MyApp
-- MyApp ∷ StateT Int IO a → MyApp a

-- GHC basically generates this:
-- instance Functor MyApp where
--   fmap ∷ ∀ a b. (a → b) → MyApp a → MyApp b
--   fmap = coerce (fmap ∷ (a → b) → StateT Int IO a → StateT Int IO b)

-- instance GHC.Base.Functor Main.MyApp where
--   GHC.Base.fmap
--     = GHC.Prim.coerce
--         @((a → b) → Control.Monad.Trans.State.Lazy.StateT GHC.Types.Int GHC.Types.IO a → Control.Monad.Trans.State.Lazy.StateT GHC.Types.Int GHC.Types.IO b)
--         @((a → b) → Main.MyApp a → Main.MyApp b)
--         (GHC.Base.fmap @(Control.Monad.Trans.State.Lazy.StateT GHC.Types.Int GHC.Types.IO)) ∷ ∀ (a ∷ TYPE GHC.Types.LiftedRep) (b ∷ TYPE GHC.Types.LiftedRep). (a → b) → Main.MyApp a → Main.MyApp b

type family Inspect t

type instance Inspect Int = Bool

type instance Inspect Age = Int

class Inspector a where
  inspect ∷ a → Inspect a

instance Inspector Int where
  inspect ∷ Int → Inspect Int -- ⇒ Int → Bool
  inspect n = n > 0

-- Is it possible to derive the Inspector instance for Age using the newtype strategy?
-- Remember that the newtype strategy here is replacing an implementation for Age with an implementation for Int.
-- An ability to do that would mean that we have a result of type Bool (mapped from `Inspect Int`) interpreted as a value of type Int (mapped from `Inspect Age`).
-- By doing so, we'd break the Haskell type system.

-- Doesn't compile:
-- GHC compiler error: "Couldn't match representation of type ‘Bool’ with that of ‘Int’ arising from a use of ‘coerce’."
-- deriving newtype instance Inspector Age -- using StandaloneDeriving extension

main ∷ IO ()
main = print $ Age 42 < Age 42 -- False
