{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Projectors (mkProjectors, proj)

-- mkProjectors ∷ [Int] → Q [Dec]
$(mkProjectors [2 .. 5])

-- This is what TH generated:
-- proj_2_0 ∷ ∀ res_a63A ty_a63B. (res_a63A, ty_a63B) → res_a63A
-- proj_2_0 = \ (x_a63C, _) → x_a63C
-- proj_2_1 ∷ ∀ ty_a63E res_a63D. (ty_a63E, res_a63D) → res_a63D
-- proj_2_1 = \ (_, x_a63F) → x_a63F
-- proj_3_0 ∷ ∀ res_a63G ty_a63H ty_a63I. (res_a63G, ty_a63H, ty_a63I) → res_a63G
-- proj_3_0 = \ (x_a63J, _, _) → x_a63J
-- proj_3_1 ∷ ∀ ty_a63L res_a63K ty_a63M. (ty_a63L, res_a63K, ty_a63M) → res_a63K
-- proj_3_1 = \ (_, x_a63N, _) → x_a63N
-- proj_3_2 ∷ ∀ ty_a63P ty_a63Q res_a63O. (ty_a63P, ty_a63Q, res_a63O) → res_a63O
-- proj_3_2 = \ (_, _, x_a63R) → x_a63R
-- proj_4_0 ∷ ∀ res_a63S ty_a63T ty_a63U ty_a63V. (res_a63S, ty_a63T, ty_a63U, ty_a63V) → res_a63S
-- proj_4_0 = \ (x_a63W, _, _, _) → x_a63W
-- proj_4_1 ∷ ∀ ty_a63Y res_a63X ty_a63Z ty_a640. (ty_a63Y, res_a63X, ty_a63Z, ty_a640) → res_a63X
-- proj_4_1 = \ (_, x_a641, _, _) → x_a641
-- proj_4_2 ∷ ∀ ty_a643 ty_a644 res_a642 ty_a645. (ty_a643, ty_a644, res_a642, ty_a645) → res_a642
-- proj_4_2 = \ (_, _, x_a646, _) → x_a646
-- proj_4_3 ∷ ∀ ty_a648 ty_a649 ty_a64a res_a647. (ty_a648, ty_a649, ty_a64a, res_a647) → res_a647
-- proj_4_3 = \ (_, _, _, x_a64b) → x_a64b
-- proj_5_0 ∷ ∀ res_a64c ty_a64d ty_a64e ty_a64f ty_a64g. (res_a64c, ty_a64d, ty_a64e, ty_a64f, ty_a64g) → res_a64c
-- proj_5_0 = \ (x_a64h, _, _, _, _) → x_a64h
-- proj_5_1 ∷ ∀ ty_a64j res_a64i ty_a64k ty_a64l ty_a64m. (ty_a64j, res_a64i, ty_a64k, ty_a64l, ty_a64m) → res_a64i
-- proj_5_1 = \ (_, x_a64n, _, _, _) → x_a64n
-- proj_5_2 ∷ ∀ ty_a64p ty_a64q res_a64o ty_a64r ty_a64s. (ty_a64p, ty_a64q, res_a64o, ty_a64r, ty_a64s) → res_a64o
-- proj_5_2 = \ (_, _, x_a64t, _, _) → x_a64t
-- proj_5_3 ∷ ∀ ty_a64v ty_a64w ty_a64x res_a64u ty_a64y. (ty_a64v, ty_a64w, ty_a64x, res_a64u, ty_a64y) → res_a64u
-- proj_5_3 = \ (_, _, _, x_a64z, _) → x_a64z
-- proj_5_4 ∷ ∀ ty_a64B ty_a64C ty_a64D ty_a64E res_a64A. (ty_a64B, ty_a64C, ty_a64D, ty_a64E, res_a64A) → res_a64A
-- proj_5_4 = \ (_, _, _, _, x_a64F) → x_a64F

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
