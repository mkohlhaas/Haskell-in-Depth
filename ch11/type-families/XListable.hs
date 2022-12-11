{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module XListable where

-- Data families provide a unified interface to different data representations!
-- Data families serve as regular data types while providing different run-time representations depending on the type parameters!

data family XList a

-- To provide an instance, we can use both data and newtype declarations.

-- [()]
--                         number of units `()` in the list
--                                        |
newtype instance XList () = XListUnit Integer

-- [Bool]
--                                 bits
--                                  |  used bits
--                                  |      |
data instance XList Bool = XBits Integer Integer

-- [Char]
data instance XList Char = XCons Char (XList Char) | XNil

-- To define functions able to work with these list representations, we have to declare
-- a type class with instances corresponding to the types we want to store in a list.
class XListable a where
  xempty ∷ XList a
  xcons ∷ a → XList a → XList a
  xheadMaybe ∷ XList a → Maybe a

-- >>> :kind XListable
-- XListable ∷ Type → Constraint

instance XListable () where
  xempty ∷ XList ()
  xempty = XListUnit 0
  xcons ∷ () → XList () → XList ()
  xcons () (XListUnit n) = XListUnit (n + 1)
  xheadMaybe ∷ XList () → Maybe ()
  xheadMaybe (XListUnit 0) = Nothing
  xheadMaybe _ = Just ()

instance XListable Bool where
  xempty ∷ XList Bool
  xempty = XBits 0 0
  xcons ∷ Bool → XList Bool → XList Bool
  xcons b (XBits bits n) = XBits (bits * 2 + if b then 1 else 0) (n + 1)
  xheadMaybe ∷ XList Bool → Maybe Bool
  xheadMaybe (XBits bits n)
    | n == 0 = Nothing
    | otherwise = Just $ odd bits -- head is the right-most bit

instance XListable Char where
  xempty ∷ XList Char
  xempty = XNil
  xcons ∷ Char → XList Char → XList Char
  xcons x xs = XCons x xs
  xheadMaybe ∷ XList Char → Maybe Char
  xheadMaybe XNil = Nothing
  xheadMaybe (XCons c _) = Just c

-- function that works with different list implementations.
testXList ∷ (Eq a, XListable a) ⇒ a → Bool
testXList a = xheadMaybe (xcons a (xcons a (xcons a xempty))) == Just a

-- >>> testXList ()
-- True
--
-- >>> testXList True
-- True
--
-- >>> testXList False
-- True
--
-- >>> testXList 'c'
-- True

