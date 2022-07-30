{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Person (Person (..), homer, spj)
import TextShow (TextShow (showb), fromString, printT)

instance TextShow Person where
  showb (Person name Nothing) = fromString name
  showb (Person name (Just age)) = fromString name <> " (" <> showb age <> ")"

main ∷ IO ()
main = do
  printT homer
  printT spj
