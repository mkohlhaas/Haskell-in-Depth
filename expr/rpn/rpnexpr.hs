{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text.IO as TIO (putStr)
import EvalRPNExcept (evalRPNMany)

rpns ∷ [Text]
rpns =
  [ "answer",
    "12 13 + 1",
    "2 +",
    "x y +",
    "1x +",
    "1 22 1 22 0 2 * * * * *",
    "10 1 2 + 2 2 1 2 * + * * * 1 x 2 + + +"
  ]

main ∷ IO ()
main = TIO.putStr $ evalRPNMany rpns [("answer", 42), ("x", 1)]

-- answer = 42
-- 12 13 + 1 Error: There are extra elements in the expression
-- 2 + Error: Not enough elements in the expression
-- x y + Error: Variable 'y' not found
-- 1x + Error: Expression component '1x' is not a number
-- 1 22 1 22 0 2 * * * * * = 0
-- 10 1 2 + 2 2 1 2 * + * * * 1 x 2 + + + = 244
