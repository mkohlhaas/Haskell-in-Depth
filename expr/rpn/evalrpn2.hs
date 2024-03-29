import EvalRPNTrans (evalRPN)
import System.Environment (getArgs)

evalPrintExpr ∷ String → IO ()
evalPrintExpr str = do
  let r = evalRPN str
  putStrLn $ str ++ " = " ++ show r

main ∷ IO ()
main = getArgs >>= evalPrintExpr . unwords
