{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Projectors where

import Language.Haskell.TH

-- >>> runQ [| \(x, _, _) -> x |]
-- LamE [TupP [VarP x_0,WildP,WildP]] (VarE x_0)

{-
-- 1st version: buliding the AST using only data constructors
proj ∷ Int → Int → ExpQ
proj n k = do
   x ← newName "x"
   let mkPat j
         | j == k = VarP x
         | otherwise = WildP
   pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
-}

{-
-- 2nd version: adding error messages
proj n k
proj ∷ Int → Int → ExpQ
 | n > 1 && 0 ⇐ k && k < n = do
    x ← newName "x"
    let mkPat j
          | j == k = VarP x
          | otherwise = WildP
    pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
 | n <= 1 = fail "Wrong number of tuple elements (must be > 1)"
 | otherwise = fail $ "Incorrect projection: "
               <> show k <> " of " <> show n <> " elements."
-}

-- 3rd version: using splices inside quotes
proj ∷ Int → Int → ExpQ
proj n k
  | n > 1 && 0 <= k && k < n = do
    x ← newName "x"
    [|\ $(mkArg x) → $(varE x)|] -- using splices inside quotes
  | n <= 1 = fail "Wrong number of tuple elements (must be > 1)."
  | otherwise = fail $ "Incorrect projection: " <> show k <> " of " <> show n <> " elements."
  where
    mkPat ∷ Name → Int → PatQ
    mkPat x j
      | j == k = varP x
      | otherwise = wildP
    mkArg ∷ Name → PatQ
    mkArg x = tupP $ map (mkPat x) [0 .. n -1]

-- type alias
-- type PatQ = Q Pat

-- >>> :type TupP
-- TupP :: [Pat] -> Pat

-- >>> :type tupP
-- tupP :: [PatQ] -> PatQ

-- >>> $(proj 3 2) ("aaa", "bbb", "ccc")
-- "ccc"

-- >>> $(proj 2 3) ("aaa", "bbb", "ccc")
-- Incorrect projection: 3 of 2 elements.

mkProjName ∷ Int → Int → Name
mkProjName n k = mkName $ "proj_" <> show n <> "_" <> show k

-- >>> mkProjName 3 2
-- proj_3_2

mkProjDec ∷ Int → Int → DecsQ
mkProjDec n k = [d|$nm = $(proj n k)|] -- Note the `[d| ... |]` Oxford brackets! They give us a list of declarations.
  where
    nm ∷ PatQ
    nm = varP $ mkProjName n k

-- >>> runQ $ mkProjDec 3 2
-- [ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_1]] (VarE x_1))) []]

-- generating a type signature not so easy as thought
-- >>> runQ [t| (a, b) -> a |]
-- Not in scope: type variable ‘a’
-- Not in scope: type variable ‘b’
-- Not in scope: type variable ‘a’

-- We need the `forall` keyword.
-- >>> runQ [t| ∀ a b. (a, b) -> a |]
-- ForallT [PlainTV a_11,PlainTV b_12] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_11)) (VarT b_12))) (VarT a_11))

-- >>> runQ [t| ∀ a b c. (a, b, c) -> a |]
-- ForallT [PlainTV a_13,PlainTV b_14,PlainTV c_15] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT a_13)) (VarT b_14)) (VarT c_15))) (VarT a_13))

-- We must generate a list of type variables, send them to forall, and then construct a
-- tuple with types and build a type for a function from a tuple to a resulting type (which
-- is called ArrowT and represents an → arrow).

-- creates our type signature
mkProjType ∷ Int → Int → DecQ
mkProjType n k = sigD nm funTy -- we need function name and its type for the signature
  where
    -- our function name
    nm ∷ Name
    nm = mkProjName n k

    funTy ∷ TypeQ
    funTy = do
      resTy ← newName "res"
      tys ← mapM (getTy resTy) [0 .. n -1]
      forallT
        (map plainTV tys)
        (pure []) -- empty list of constraints
        [t|$(mkTuple tys) → $(varT resTy)|] -- Oxford bracket for type declarations

-- >>> runQ funTy
-- ForallT [PlainTV ty_17,PlainTV ty_18,PlainTV res_16] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_17)) (VarT ty_18)) (VarT res_16))) (VarT res_16))

    getTy ∷ Name → Int → Q Name
    getTy resTy j
      | k == j = pure resTy
      | otherwise = newName "ty"

    mkTuple ∷ [Name] → TypeQ
    mkTuple tys = pure $ foldl addApp (TupleT n) tys

    addApp ∷ Type → Name → Type
    addApp acc_ty ty = AppT acc_ty (VarT ty)

mkProjectors ∷ [Int] → Q [Dec]
mkProjectors = fmap concat . mapM projectors
  where
    projectors ∷ Int → Q [Dec]
    projectors n = concat <$> mapM (mkProj n) [0 .. n -1]

    mkProj ∷ Int → Int → Q [Dec]
    mkProj n k = (:) <$> mkProjType n k <*> mkProjDec n k

-- >>> runQ $ mkProjectors [2 .. 5]
-- [SigD proj_2_0 (ForallT [PlainTV res_20,PlainTV ty_21] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT res_20)) (VarT ty_21))) (VarT res_20))),ValD (VarP proj_2_0) (NormalB (LamE [TupP [VarP x_22,WildP]] (VarE x_22))) [],SigD proj_2_1 (ForallT [PlainTV ty_24,PlainTV res_23] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT ty_24)) (VarT res_23))) (VarT res_23))),ValD (VarP proj_2_1) (NormalB (LamE [TupP [WildP,VarP x_25]] (VarE x_25))) [],SigD proj_3_0 (ForallT [PlainTV res_26,PlainTV ty_27,PlainTV ty_28] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT res_26)) (VarT ty_27)) (VarT ty_28))) (VarT res_26))),ValD (VarP proj_3_0) (NormalB (LamE [TupP [VarP x_29,WildP,WildP]] (VarE x_29))) [],SigD proj_3_1 (ForallT [PlainTV ty_31,PlainTV res_30,PlainTV ty_32] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_31)) (VarT res_30)) (VarT ty_32))) (VarT res_30))),ValD (VarP proj_3_1) (NormalB (LamE [TupP [WildP,VarP x_33,WildP]] (VarE x_33))) [],SigD proj_3_2 (ForallT [PlainTV ty_35,PlainTV ty_36,PlainTV res_34] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_35)) (VarT ty_36)) (VarT res_34))) (VarT res_34))),ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_37]] (VarE x_37))) [],SigD proj_4_0 (ForallT [PlainTV res_38,PlainTV ty_39,PlainTV ty_40,PlainTV ty_41] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT res_38)) (VarT ty_39)) (VarT ty_40)) (VarT ty_41))) (VarT res_38))),ValD (VarP proj_4_0) (NormalB (LamE [TupP [VarP x_42,WildP,WildP,WildP]] (VarE x_42))) [],SigD proj_4_1 (ForallT [PlainTV ty_44,PlainTV res_43,PlainTV ty_45,PlainTV ty_46] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_44)) (VarT res_43)) (VarT ty_45)) (VarT ty_46))) (VarT res_43))),ValD (VarP proj_4_1) (NormalB (LamE [TupP [WildP,VarP x_47,WildP,WildP]] (VarE x_47))) [],SigD proj_4_2 (ForallT [PlainTV ty_49,PlainTV ty_50,PlainTV res_48,PlainTV ty_51] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_49)) (VarT ty_50)) (VarT res_48)) (VarT ty_51))) (VarT res_48))),ValD (VarP proj_4_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_52,WildP]] (VarE x_52))) [],SigD proj_4_3 (ForallT [PlainTV ty_54,PlainTV ty_55,PlainTV ty_56,PlainTV res_53] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_54)) (VarT ty_55)) (VarT ty_56)) (VarT res_53))) (VarT res_53))),ValD (VarP proj_4_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_57]] (VarE x_57))) [],SigD proj_5_0 (ForallT [PlainTV res_58,PlainTV ty_59,PlainTV ty_60,PlainTV ty_61,PlainTV ty_62] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT res_58)) (VarT ty_59)) (VarT ty_60)) (VarT ty_61)) (VarT ty_62))) (VarT res_58))),ValD (VarP proj_5_0) (NormalB (LamE [TupP [VarP x_63,WildP,WildP,WildP,WildP]] (VarE x_63))) [],SigD proj_5_1 (ForallT [PlainTV ty_65,PlainTV res_64,PlainTV ty_66,PlainTV ty_67,PlainTV ty_68] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_65)) (VarT res_64)) (VarT ty_66)) (VarT ty_67)) (VarT ty_68))) (VarT res_64))),ValD (VarP proj_5_1) (NormalB (LamE [TupP [WildP,VarP x_69,WildP,WildP,WildP]] (VarE x_69))) [],SigD proj_5_2 (ForallT [PlainTV ty_71,PlainTV ty_72,PlainTV res_70,PlainTV ty_73,PlainTV ty_74] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_71)) (VarT ty_72)) (VarT res_70)) (VarT ty_73)) (VarT ty_74))) (VarT res_70))),ValD (VarP proj_5_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_75,WildP,WildP]] (VarE x_75))) [],SigD proj_5_3 (ForallT [PlainTV ty_77,PlainTV ty_78,PlainTV ty_79,PlainTV res_76,PlainTV ty_80] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_77)) (VarT ty_78)) (VarT ty_79)) (VarT res_76)) (VarT ty_80))) (VarT res_76))),ValD (VarP proj_5_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_81,WildP]] (VarE x_81))) [],SigD proj_5_4 (ForallT [PlainTV ty_83,PlainTV ty_84,PlainTV ty_85,PlainTV ty_86,PlainTV res_82] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_83)) (VarT ty_84)) (VarT ty_85)) (VarT ty_86)) (VarT res_82))) (VarT res_82))),ValD (VarP proj_5_4) (NormalB (LamE [TupP [WildP,WildP,WildP,WildP,VarP x_87]] (VarE x_87))) []]

-- This is what TH generated:
-- proj_2_0 ∷ forall res_a63A ty_a63B. (res_a63A, ty_a63B) → res_a63A
-- proj_2_0 = \ (x_a63C, _) → x_a63C
-- proj_2_1 ∷ forall ty_a63E res_a63D. (ty_a63E, res_a63D) → res_a63D
-- proj_2_1 = \ (_, x_a63F) → x_a63F
-- proj_3_0 ∷ forall res_a63G ty_a63H ty_a63I. (res_a63G, ty_a63H, ty_a63I) → res_a63G
-- proj_3_0 = \ (x_a63J, _, _) → x_a63J
-- proj_3_1 ∷ forall ty_a63L res_a63K ty_a63M. (ty_a63L, res_a63K, ty_a63M) → res_a63K
-- proj_3_1 = \ (_, x_a63N, _) → x_a63N
-- proj_3_2 ∷ forall ty_a63P ty_a63Q res_a63O. (ty_a63P, ty_a63Q, res_a63O) → res_a63O
-- proj_3_2 = \ (_, _, x_a63R) → x_a63R
-- proj_4_0 ∷ forall res_a63S ty_a63T ty_a63U ty_a63V. (res_a63S, ty_a63T, ty_a63U, ty_a63V) → res_a63S
-- proj_4_0 = \ (x_a63W, _, _, _) → x_a63W
-- proj_4_1 ∷ forall ty_a63Y res_a63X ty_a63Z ty_a640. (ty_a63Y, res_a63X, ty_a63Z, ty_a640) → res_a63X
-- proj_4_1 = \ (_, x_a641, _, _) → x_a641
-- proj_4_2 ∷ forall ty_a643 ty_a644 res_a642 ty_a645. (ty_a643, ty_a644, res_a642, ty_a645) → res_a642
-- proj_4_2 = \ (_, _, x_a646, _) → x_a646
-- proj_4_3 ∷ forall ty_a648 ty_a649 ty_a64a res_a647. (ty_a648, ty_a649, ty_a64a, res_a647) → res_a647
-- proj_4_3 = \ (_, _, _, x_a64b) → x_a64b
-- proj_5_0 ∷ forall res_a64c ty_a64d ty_a64e ty_a64f ty_a64g. (res_a64c, ty_a64d, ty_a64e, ty_a64f, ty_a64g) → res_a64c
-- proj_5_0 = \ (x_a64h, _, _, _, _) → x_a64h
-- proj_5_1 ∷ forall ty_a64j res_a64i ty_a64k ty_a64l ty_a64m. (ty_a64j, res_a64i, ty_a64k, ty_a64l, ty_a64m) → res_a64i
-- proj_5_1 = \ (_, x_a64n, _, _, _) → x_a64n
-- proj_5_2 ∷ forall ty_a64p ty_a64q res_a64o ty_a64r ty_a64s. (ty_a64p, ty_a64q, res_a64o, ty_a64r, ty_a64s) → res_a64o
-- proj_5_2 = \ (_, _, x_a64t, _, _) → x_a64t
-- proj_5_3 ∷ forall ty_a64v ty_a64w ty_a64x res_a64u ty_a64y. (ty_a64v, ty_a64w, ty_a64x, res_a64u, ty_a64y) → res_a64u
-- proj_5_3 = \ (_, _, _, x_a64z, _) → x_a64z
-- proj_5_4 ∷ forall ty_a64B ty_a64C ty_a64D ty_a64E res_a64A. (ty_a64B, ty_a64C, ty_a64D, ty_a64E, res_a64A) → res_a64A
-- proj_5_4 = \ (_, _, _, _, x_a64F) → x_a64F
