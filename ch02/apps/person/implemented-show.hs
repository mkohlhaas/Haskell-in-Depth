{-# LANGUAGE InstanceSigs #-}

import Person (Person (..), homer, spj)

instance Show Person where
  show ∷ Person → String
  show (Person name Nothing) = name
  show (Person name (Just age)) = name ++ " (" ++ show age ++ ")"

instance Eq Person where
  (==) ∷ Person → Person → Bool
  (Person name1 age1) == (Person name2 age2) = name1 == name2 && age1 == age2

-- >>> show homer
-- >>> show spj
-- >>> homer == homer
-- >>> homer == spj
-- "Homer Simpson (39)"
-- "Simon Peyton Jones"
-- True
-- False

main ∷ IO ()
main = do
  print homer
  print spj
