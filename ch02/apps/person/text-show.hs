{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

import Person (Person (..), homer, spj)
import TextShow (TextShow (showb), fromString, printT, Builder)

-- >>> :info TextShow
-- type TextShow :: * -> Constraint
-- class TextShow a where
--   showbPrec :: Int -> a -> Builder
--   showb :: a -> Builder
--   {-# MINIMAL showbPrec | showb #-}

instance TextShow Person where
  showb :: Person -> Builder
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) = fromString name <> " (" <> showb age <> ")"

instance Eq Person where
  (==) ∷ Person → Person → Bool
  (Person name1 age1) == (Person name2 age2) = name1 == name2 && age1 == age2

-- >>> showb homer
-- >>> showb spj
-- >>> homer == homer
-- >>> homer == spj
-- "Homer Simpson (39)"
-- "Simon Peyton Jones"
-- True
-- False

main ∷ IO ()
main = do
  printT homer
  printT spj
