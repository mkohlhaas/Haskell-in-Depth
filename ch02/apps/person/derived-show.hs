{-# LANGUAGE StandaloneDeriving #-}

import Person (Person (..), homer, spj)

deriving instance Show Person

deriving instance Read Person

deriving instance Eq Person

-- >>> show homer
-- >>> show spj
-- >>> homer == homer
-- >>> homer == spj
-- "Person \"Homer Simpson\" (Just 39)"
-- "Person \"Simon Peyton Jones\" Nothing"
-- True
-- False

main ∷ IO ()
main = do
  print homer
  print spj
