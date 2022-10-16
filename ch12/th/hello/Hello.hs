{-# LANGUAGE TemplateHaskell #-}

module Hello where

import Language.Haskell.TH

-- >>> runQ [| 1 + 2 |]
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

-- >>> :type [| 1 + 2 |]
-- [| 1 + 2 |] :: ExpQ

-- Let's construct an ExpQ by hand. Note the usage of mkName!
-- >>> InfixE (Just (LitE (IntegerL 1))) (VarE (mkName "+")) (Just (LitE (IntegerL 2)))
-- InfixE (Just (LitE (IntegerL 1))) (VarE +) (Just (LitE (IntegerL 2)))

-- >>> $(pure $ InfixE (Just (LitE (IntegerL 1))) (VarE (mkName "+")) (Just (LitE (IntegerL 2))))
-- 3

-- >>> runQ [| 42 |]
-- LitE (IntegerL 42)

-- >>> $(pure $ LitE (IntegerL 42))
-- 42

-- We need `pure`.
-- >>> $(LitE (IntegerL 42))
-- Couldn't match type ‘Exp’ with ‘Q Exp’
-- Expected type: ExpQ
--   Actual type: Exp

-- hello ∷ Q Exp
hello ∷ ExpQ
hello = [|putStrLn "Hello world"|] -- quoting
