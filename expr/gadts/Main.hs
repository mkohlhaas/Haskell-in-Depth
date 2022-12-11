{-# LANGUAGE GADTs #-}

-- GADTs add type control to arithmetic expressions.

data Expr' a
  = Lit' !a
  | Add' !(Expr' a) !(Expr' a)
  | Mult' !(Expr' a) !(Expr' a)

myeval' ∷ Num a ⇒ Expr' a → a
myeval' (Lit' e) = e
myeval' (Add' e1 e2) = myeval' e1 + myeval' e2
myeval' (Mult' e1 e2) = myeval' e1 * myeval' e2

-- GADT Syntax (exactly the same as Expr')
-- Always returns an `Expr'' a`. (`{-# LANGUAGE GADTSyntax #-}` sufficient.)
data Expr'' a where
  Lit'' ∷ a → Expr'' a
  Add'' ∷ Expr'' a → Expr'' a → Expr'' a
  Mult'' ∷ Expr'' a → Expr'' a → Expr'' a

-- How would we extend this `Expr a` type to Boolean expressions?
-- They are not numbers, so we cannot simply use the a type variable for Bool because then we'd have to deal with additions and multiplications of Booleans.
-- Moreover, we’d like to have conditional expressions once we have Booleans.

-- GADT (expanded to support Booleans).
-- GADTs allow data constructors to return a data type parameterized by specific types!
-- In comparison, e.g. a `Maybe a` always returns a `Maybe a` for all `a`!
-- Returns `Expr a` for `Num a`, `Expr a` for all `a` and `Expr Bool`.
data Expr a where
  Add ∷ Num a ⇒ Expr a → Expr a → Expr a ------- SAME
  Mult ∷ Num a ⇒ Expr a → Expr a → Expr a ------ SAME
  NumLit ∷ Num a ⇒ a → Expr a ------------------ CHANGED
  BoolLit ∷ Bool → Expr Bool ------------------- NEW
  IsZero ∷ (Num a, Eq a) ⇒ Expr a → Expr Bool -- NEW
  If ∷ Expr Bool → Expr a → Expr a → Expr a ---- NEW

myeval ∷ Expr a → a
myeval (NumLit e) = e
myeval (BoolLit b) = b
myeval (Add e1 e2) = myeval e1 + myeval e2
myeval (Mult e1 e2) = myeval e1 * myeval e2
myeval (IsZero e) = myeval e == 0
myeval (If e e1 e2) = myeval (if myeval e then e1 else e2)

expr1 ∷ Expr Integer
expr1 = Add (NumLit 5) (NumLit (-5))

expr2 ∷ Expr Double
expr2 = If (IsZero expr1) (NumLit 0.5) (NumLit 1)

-- >>> myeval expr1
-- 0

-- >>> myeval expr2
-- 0.5

-- Trying to construct a wrong expression.
-- Compiler Error: No instance for (Num Bool) arising from a use of ‘IsZero’
-- expr3 = IsZero (BoolLit True)

-- It is hard to construct `Expr a`s programmatically.
-- Remember, we have an implicit `∀ a` in the type:
-- It is impossible to define e.g. a `String → Expr a` function, because with this type, we should provide the result of any type.
-- Depending on the given String, it can be either Bool or some numeric type.

-- existential wrapper as a workaround
data SomeExpr where
  Some ∷ Expr a → SomeExpr

-- Then we should think of defining e.g. a `String → SomeExpr` function.

main ∷ IO ()
main = do
  print $ myeval expr1 -- 0
  print $ myeval expr2 -- 0.5
