{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

-- sum
data a + b = Inl a | Inr b
  deriving Show

-- product
data a * b = a :*: b -- infix data constructor names are required to start with ':'
  deriving Show

infixl 6 +
infixl 7 *

-- getters for products
first :: a * b -> a
first (a :*: _) = a

second :: a * b -> b
second (_ :*: b) = b

val1 :: Int + Bool * Bool
val1 = Inl 0

val2 :: Int + Bool * Bool
val2 = Inr (True :*: False)

-- store a point in 1D-, 2D- or 3D-space
type Point a = a + a * a + a * a * a

-- origin in 1D-space
zero1D :: Point Int
zero1D = Inl (Inl 0)

-- origin in 2D-space
zero2D :: Point Int
zero2D = Inl (Inr (0 :*: 0))

-- origin in 3D-space?
-- zero3D :: Point Int
-- zero3D = Inl (0 :*: 0 :*: 0)

main :: IO ()
main = do
  print val1
  print val2
  print zero2D
