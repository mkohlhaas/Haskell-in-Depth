{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Projectors

$(mkProjectors [2 .. 10])

main :: IO ()
main = do
  -- Incorrect projection: 4 of 3 elements• In the untyped splice: $(proj 3 4)
  -- putStrLn $ $(proj 3 4) ("aaa", "bbb", "ccc")
  putStrLn $ $(proj 3 1) (undefined, "Success!", undefined)
  putStrLn $ $(proj 4 2) (undefined, undefined, "Success!", undefined)
  putStrLn $ $(proj 5 4) (undefined, undefined, undefined, undefined, "Success!")

  putStrLn $ proj_3_1 (undefined, "Success!", undefined)
  putStrLn $ proj_4_2 (undefined, undefined, "Success!", undefined)
  putStrLn $ proj_5_4 (undefined, undefined, undefined, undefined, "Success!")
