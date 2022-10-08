{-# LANGUAGE TypeFamilies #-}

module XListable where

-- Data families provide a unified interface to different data representations!
-- Data families serve as regular data types while providing different run-time representations depending on the type parameters!
data family XList a

-- To provide an instance, we can use both data and newtype declarations (with only one data constructor with one field for the latter).

-- list of units `()`
newtype instance XList () = XListUnit Integer -- Integer is the number of units `()` in the list

-- list of bools
data instance XList Bool = XBits Integer Integer -- first Integer = bits; second Integer = number of used bits

-- list of chars
data instance XList Char = XCons Char (XList Char) | XNil

-- To define functions able to work with these list representations, we have to declare
-- a type class with instances corresponding to the types we want to store in a list.
class XListable a where
  xempty ∷ XList a
  xcons ∷ a → XList a → XList a
  xheadMaybe ∷ XList a → Maybe a

instance XListable () where
  xempty = XListUnit 0
  xcons () (XListUnit n) = XListUnit (n + 1)
  xheadMaybe (XListUnit 0) = Nothing
  xheadMaybe _ = Just ()

instance XListable Bool where
  xempty = XBits 0 0
  xcons b (XBits bits n) = XBits (bits * 2 + if b then 1 else 0) (n + 1)
  xheadMaybe (XBits bits n)
    | n == 0 = Nothing
    | otherwise = Just $ odd bits

instance XListable Char where
  xempty = XNil
  xcons x xs = XCons x xs
  xheadMaybe XNil = Nothing
  xheadMaybe (XCons c _) = Just c

-- Now we can define functions that work with different list implementations without knowing which one is actually chosen.
testXList ∷ (Eq a, XListable a) ⇒ a → Bool
testXList a = xheadMaybe (xcons a xempty) == Just a

--  Works with differently typed arguments:

-- |
-- >>> testXList ()
-- True

-- |
-- >>> testXList True
-- True

-- |
-- >>> testXList False
-- True
