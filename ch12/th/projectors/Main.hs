{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Projectors (mkProjectors, proj)

-- mkProjectors ∷ [Int] → Q [Dec]
$(mkProjectors [2 .. 5])

-- This is what TH generated (cleaned up, ommitting type signatures):
-- proj_2_0 = \ (x, _) → x
-- proj_2_1 = \ (_, x) → x
-- proj_3_0 = \ (x, _, _) → x
-- proj_3_1 = \ (_, x, _) → x
-- proj_3_2 = \ (_, _, x) → x
-- proj_4_0 = \ (x, _, _, _) → x
-- proj_4_1 = \ (_, x, _, _) → x
-- proj_4_2 = \ (_, _, x, _) → x
-- proj_4_3 = \ (_, _, _, x) → x
-- proj_5_0 = \ (x, _, _, _, _) → x
-- proj_5_1 = \ (_, x, _, _, _) → x
-- proj_5_2 = \ (_, _, x, _, _) → x
-- proj_5_3 = \ (_, _, _, x, _) → x
-- proj_5_4 = \ (_, _, _, _, x) → x

main ∷ IO ()
main = do
  -- Splicing
  putStrLn $ $(proj 3 1) (undefined, "Success!", undefined)
  putStrLn $ $(proj 4 2) (undefined, undefined, "Success!", undefined)
  putStrLn $ $(proj 5 4) (undefined, undefined, undefined, undefined, "Success!")
  -- Calling generated functions
  putStrLn $ proj_3_1 (undefined, "Success!", undefined)
  putStrLn $ proj_4_2 (undefined, undefined, "Success!", undefined)
  putStrLn $ proj_5_4 (undefined, undefined, undefined, undefined, "Success!")
  -- GHC error: "Incorrect projection: 4 of 3 elements. In the untyped splice: $(proj 3 4)"
  -- putStrLn $ $(proj 3 4) ("aaa", "bbb", "ccc") -- Incorrect projection: 4 of 3 elements• In the untyped splice: $(proj 3 4)
