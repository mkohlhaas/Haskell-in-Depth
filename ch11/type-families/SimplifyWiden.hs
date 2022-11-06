{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module SimplifyWiden where

---------------------------------------------
-- Type synonym families (Open and Closed) --
---------------------------------------------

-- A synonym type family describes how types are mapped onto other types.
-- To implement a function that uses a type family in its type, we define a type class and
-- instances or call another function that we've already defined in a type class.

-------------------------
-- 1. Open type family --
-------------------------

-- Open type families are quite limited.
-- For example, there is no way to say something like "simplify all other types to String".
-- The reason is straightforward: a user should have an ability to add more type family instances later.
-- A catchall instance would conflict with them.

type family Simplify t

-- Suppose we want to simplify the types we use in a program.
-- For example, we are porting an application to some other programming language with minimal types, say, Integer and String.

-- map types to other types; in this case to Integer or String
type instance Simplify Integer = Integer

type instance Simplify Int = Integer

type instance Simplify Double = Integer

type instance Simplify String = String

type instance Simplify Char = String

-- type instance Simplify Bool = Integer

-- author's suggestion for Bool
type instance Simplify Bool = String

-- It is not enough to describe this transformation at the level of types, we also have to process our data.
-- As usual, with functions that should be able to process data of different types, we have to define a type class.
class Simplifier t where
  simplify ∷ t → Simplify t

instance Simplifier Integer where
  simplify ∷ Integer → Simplify Integer -- Integer → Integer
  simplify = id

instance Simplifier Int where
  simplify ∷ Int → Simplify Int -- Int → Integer
  simplify = fromIntegral

instance Simplifier Double where
  simplify ∷ Double → Simplify Double -- Double → Integer
  simplify = round

instance Simplifier String where
  simplify ∷ String → Simplify String -- String → String
  simplify = id

-- instance Simplifier Bool where
-- simplify ∷ Bool → Simplify Bool -- Bool → Integer
--   simplify False = 0
--   simplify True = 1

-- author's suggestion for Bool
instance Simplifier Bool where
  simplify ∷ Bool → Simplify Bool -- Bool → String
  simplify = show

instance Simplifier Char where
  simplify ∷ Char → Simplify Char -- Char → String
  simplify = (: "")

-- >>> :kind! Simplify Bool
-- Simplify Bool ∷ Type
-- = String

-- >>> simplify (3.14 ∷ Double)
-- 3

-- >>> simplify True
-- "True"

-- >>> simplify 'x'
-- "x"

---------------------------
-- 2. Closed type family --
---------------------------

-- Note the keyword `where`.
type family Widen a where
  Widen Bool = Int
  Widen Int = Integer
  Widen Char = String
  Widen t = String -- catchall: widen all other types to String

class Widener a where
  widen ∷ a → Widen a

instance Widener Bool where
  widen ∷ Bool → Widen Bool
  widen False = 0
  widen True = 1

instance Widener Int where
  widen ∷ Int → Widen Int
  widen a = fromIntegral a

instance Widener Char where
  widen ∷ Char → Widen Char
  widen c = [c]

instance Widener Double where
  widen ∷ Double → Widen Double
  widen = show

instance Widener Integer where
  widen ∷ Integer → Widen Integer
  widen = show

-- >>> :type widen False
-- widen False :: Int
--
-- >>> :type widen (1 ∷ Int)
-- widen (1 ∷ Int) :: Integer

-- >>> widen False
-- 0

-- >>> widen True
-- 1

-- >>> widen 'x'
-- "x"

-- >>> widen (1 ∷ Int)
-- 1

-- >>> widen (1 ∷ Double)
-- "1.0"

-- >>> widen (1 ∷ Integer)
-- "1"

-- >>> widen (widen True)
-- 1
