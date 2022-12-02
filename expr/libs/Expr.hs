{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Expr where

import TextShow (Builder, TextShow (showb, showt, showbPrec), showbParen, toString)

data Expr a
  = Lit !a
  | Add !(Expr a) !(Expr a)
  | Mult !(Expr a) !(Expr a)
  deriving (Show, Read)

myeval ∷ Num a ⇒ Expr a → a
myeval (Lit e) = e
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2

-- >>> myeval expr1
-- 55

-- >>> myeval expr2
-- 32

instance TextShow a ⇒ TextShow (Expr a) where
  showbPrec ∷ TextShow a ⇒ Int → Expr a → Builder
  showbPrec p e =
    case e of
      Lit a → showb a
      Add e1 e2 → showbHelper p 0 " + " e1 e2
      Mult e1 e2 → showbHelper p 1 " * " e1 e2
    where
      showbHelper outerPrec thisPrec op e1 e2 = showbParen (outerPrec > thisPrec) $ showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2

expr1, expr2 ∷ Expr Int
expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)
expr2 = Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2)) (Add (Lit 2) (Mult (Lit 2) (Add (Lit 1) (Lit 2)))))) (Add (Lit 1) (Mult (Lit 3) (Lit 2)))

-- Not really helpful for end-users.
-- >>> show expr1
-- "Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)"
--
-- >>> showb expr1
-- "(2 + 3 * 3) * 5"
--
-- >>> showb expr2
-- "1 + (1 + 2) * (2 + 2 * (1 + 2)) + 1 + 3 * 2"
--
-- convert expressions to Text or String:
-- >>> showt expr1 -- showt returns Text by using TextShow.
-- "(2 + 3 * 3) * 5"
--
-- >>> toString $ showb expr1 -- toString converts Builder to String.
-- "(2 + 3 * 3) * 5"

