{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Person
import TextShow

instance TextShow Person where
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) = fromString name <> " (" <> showb age <> ")"

main ∷ IO ()
main = do
  printT homer
  printT spj
