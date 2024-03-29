import Data.List (intersperse)
import Expr (Expr (..))
import ShuntingYard (convertToExpr)

-- Converting expressions to prefix and postfix forms

data ExprForm = Prefix | Postfix

exprTo ∷ Show a ⇒ ExprForm → Expr a → String
exprTo _ (Lit a) = show a
exprTo form (Add e1 e2) = binOp "+" form e1 e2
exprTo form (Mult e1 e2) = binOp "*" form e1 e2

binOp ∷ Show a ⇒ String → ExprForm → Expr a → Expr a → String
binOp op form e1 e2 = unwords $ args form
  where
    e1', e2' ∷ String
    e1' = exprTo form e1
    e2' = exprTo form e2

    args ∷ ExprForm → [String]
    args Prefix = [op, e1', e2']
    args Postfix = [e1', e2', op]

-- >>> convertToExpr "(2+3*3)*5"
-- Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

-- >>> exprTo Postfix $ convertToExpr "(2+3*3)*5"
-- "2 3 3 * + 5 *"

-- >>> exprTo Prefix $ convertToExpr "(2+3*3)*5"
-- "* + 2 * 3 3 5"

-- >>> strs = ["42", "12 + 13", "(2+3*3)*5"]
-- >>> map convertToExpr strs
-- [Lit 42,Add (Lit 12) (Lit 13),Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)]

-- >>> strs = ["42", "12 + 13", "(2+3*3)*5", "1+(1+2)*(2+2*(1+2))+1+3*2"]
-- >>> map (exprTo Prefix . convertToExpr) strs
-- ["42","+ 12 13","* + 2 * 3 3 5","+ 1 + * + 1 2 + 2 * 2 + 1 2 + 1 * 3 2"]

-- >>> strs = ["42", "12 + 13", "(2+3*3)*5", "1+(1+2)*(2+2*(1+2))+1+3*2"]
-- >>> map (exprTo Postfix . convertToExpr) strs
-- ["42","12 13 +","2 3 3 * + 5 *","1 1 2 + 2 2 1 2 + * + * 1 3 2 * + + +"]

main ∷ IO ()
main = mapM_ printExpr strs
  where
    strs = ["42", "12 + 13", "(2+3*3)*5", "1+(1+2)*(2+2*(1+2))+1+3*2"]

    printExpr ∷ String → IO ()
    printExpr str = do
      putStrLn $ "Expression: " ++ str
      let expr = convertToExpr str
      putStrLn $ "Prefix form: " ++ exprTo Prefix expr
      putStrLn $ "Postfix form: " ++ exprTo Postfix expr
      putStrLn "-------------"

-- Output:
--
-- Expression: 42
-- Prefix form: 42
-- Postfix form: 42
-- -------------
-- Expression: 12 + 13
-- Prefix form: + 12 13
-- Postfix form: 12 13 +
-- -------------
-- Expression: (2+3*3)*5
-- Prefix form: * + 2 * 3 3 5
-- Postfix form: 2 3 3 * + 5 *
-- -------------
-- Expression: 1+(1+2)*(2+2*(1+2))+1+3*2
-- Prefix form: + 1 + * + 1 2 + 2 * 2 + 1 2 + 1 * 3 2
-- Postfix form: 1 1 2 + 2 2 1 2 + * + * 1 3 2 * + + +
-- -------------
