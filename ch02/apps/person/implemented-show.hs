{-# LANGUAGE UnicodeSyntax #-}

import Person (Person (..), homer, spj)

instance Show Person where
  show (Person name Nothing) = name
  show (Person name (Just age)) = name ++ " (" ++ show age ++ ")"

main ∷ IO ()
main = do
  print homer
  print spj
