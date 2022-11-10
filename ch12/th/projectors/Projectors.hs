{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Projectors where

import Language.Haskell.TH

-- Let's see what GHC will generate!
-- >>> runQ [| \(x, _, _) -> x |]
-- LamE [TupP [VarP x_0,WildP,WildP]] (VarE x_0)

-- newName - generate a new name which cannot be captured
-- >>> :type newName
-- newName ∷ String → Q Name

{-
-- 1st version: building the AST using only data constructors.
proj ∷ Int → Int → ExpQ
proj n k = do
   x ← newName "x"
   let mkPat j
         | j == k = VarP x
         | otherwise = WildP
   pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
-}

-- >>> runQ $ proj 2 1
-- LamE [TupP [WildP,VarP x_4]] (VarE x_4)

-- >>> runQ $ proj 10 5
-- LamE [TupP [WildP,WildP,WildP,WildP,WildP,VarP x_5,WildP,WildP,WildP,WildP]] (VarE x_5)

-- >>> $(proj 2 3) ("aaa", "bbb", "ccc")
-- The exact Name ‘x_a11qz’ is not in scope
--   Probable cause: you used a unique Template Haskell name (NameU),
--   perhaps via newName, but did not bind it
--   If that's it, then -ddump-splices might be useful

{-
-- 2nd version: adding error messages.
proj ∷ Int → Int → ExpQ
proj n k
 | n > 1 && 0 <= k && k < n = do
    x ← newName "x"
    let mkPat j
          | j == k = VarP x
          | otherwise = WildP
    pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
 | n <= 1 = fail "Wrong number of tuple elements (must be > 1)"
 | otherwise = fail $ "Incorrect projection: " <> show k <> " of " <> show n <> " elements."
-}

-- >>> runQ $ proj 2 1
-- LamE [TupP [WildP,VarP x_6]] (VarE x_6)

-- >>> runQ $ proj 10 5
-- LamE [TupP [WildP,WildP,WildP,WildP,WildP,VarP x_7,WildP,WildP,WildP,WildP]] (VarE x_7)

-- >>> $(proj 2 3) ("aaa", "bbb", "ccc")
-- Incorrect projection: 3 of 2 elements.

-- {-
-- 3rd version: using splices inside quotes.
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
-- -}

-- Note that we're using varE, varP, wildP, and tupP instead of VarE, VarP, WildP, and TupP.
-- These functions return their results in the Q monad already.
-- So they are composed better with the $(...) splice operator that expects `Q a` values inside it.

-- >>> :type $(proj 3 2)
-- $(proj 3 2) ∷ (a, b, c) → c

-- >>> runQ $ proj 2 1
-- LamE [TupP [WildP,VarP x_9]] (VarE x_9)

-- >>> runQ $ proj 10 5
-- LamE [TupP [WildP,WildP,WildP,WildP,WildP,VarP x_10,WildP,WildP,WildP,WildP]] (VarE x_10)

-- type alias
-- type PatQ = Q Pat

-- >>> :type TupP
-- TupP ∷ [Pat] → Pat

-- >>> :type tupP
-- tupP ∷ [PatQ] → PatQ

-- >>> $(proj 3 2) ("aaa", "bbb", "ccc")
-- "ccc"

-- >>> $(proj 2 3) ("aaa", "bbb", "ccc")
-- Incorrect projection: 3 of 2 elements.

-- >>> $(proj 1 3) ("aaa", "bbb", "ccc")
-- Wrong number of tuple elements (must be > 1).

mkProjName ∷ Int → Int → Name
mkProjName n k = mkName $ "proj_" <> show n <> "_" <> show k

-- >>> mkProjName 3 2
-- proj_3_2

mkProjDec ∷ Int → Int → DecsQ
mkProjDec n k = [d|$nm = $(proj n k)|] -- Note the `[d| ... |]` Oxford brackets! Gives a list of declarations.
  where
    nm ∷ PatQ
    nm = varP $ mkProjName n k

-- >>> runQ $ mkProjDec 3 2
-- [ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_1]] (VarE x_1))) []]

--          mkProjName                      proj 3 2
--             |                              |
-- [ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_1]] (VarE x_1))) []]
--   |                      |
-- [d|..]                 [d|..]

-- Type signatures need explicit ∀s.
-- There are no implicit ∀s as in standard Haskell code.
-- >>> runQ [t| (a, b) -> a |]
-- Not in scope: type variable ‘a’
-- Not in scope: type variable ‘b’
-- Not in scope: type variable ‘a’

-- We need the `forall` keyword.
-- >>> runQ [t| forall a b. (a, b) -> a |]
-- ForallT [PlainTV a_11,PlainTV b_12] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_11)) (VarT b_12))) (VarT a_11))

-- >>> runQ [t| ∀ a b c. (a, b, c) -> a |]
-- ForallT [PlainTV a_13,PlainTV b_14,PlainTV c_15] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT a_13)) (VarT b_14)) (VarT c_15))) (VarT a_13))

-- Side note: Type signatures are not necessary as they will be inferred by GHC.

-- creates our type signature
mkProjType ∷ Int → Int → DecQ
mkProjType n k = sigD nm funTy -- we need a function name and its type for the signature
  where
    nm ∷ Name ------ the function name
    nm = mkProjName n k
    funTy ∷ TypeQ -- the function type
    funTy = do
      resTy ← newName "res"
      tys ← mapM (getTy resTy) [0 .. n -1]
      forallT
        (map plainTV tys)
        (pure []) -- empty list of constraints
        [t|$(mkTuple tys) → $(varT resTy)|]
    getTy ∷ Name → Int → Q Name
    getTy resTy j
      | k == j = pure resTy
      | otherwise = newName "ty"
    mkTuple ∷ [Name] → TypeQ
    mkTuple tys = pure $ foldl addApp (TupleT n) tys
    addApp ∷ Type → Name → Type
    addApp acc_ty ty = AppT acc_ty (VarT ty)

-- >>> runQ funTy
-- ForallT [PlainTV ty_17,PlainTV ty_18,PlainTV res_16] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_17)) (VarT ty_18)) (VarT res_16))) (VarT res_16))

-- >>> runQ $ mkProjType 3 1
-- SigD proj_3_1 (ForallT [PlainTV ty_19,PlainTV res_18,PlainTV ty_20] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_19)) (VarT res_18)) (VarT ty_20))) (VarT res_18)))

mkProjectors ∷ [Int] → Q [Dec]
mkProjectors = fmap concat . mapM projectors
  where
    projectors ∷ Int → Q [Dec]
    projectors n = concat <$> mapM (mkProj n) [0 .. n -1]
    mkProj ∷ Int → Int → Q [Dec]
    mkProj n k = (:) <$> mkProjType n k <*> mkProjDec n k

-- >>> runQ $ mkProjectors [2 .. 5]
-- [SigD proj_2_0 (ForallT [PlainTV res_20,PlainTV ty_21] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT res_20)) (VarT ty_21))) (VarT res_20))),ValD (VarP proj_2_0) (NormalB (LamE [TupP [VarP x_22,WildP]] (VarE x_22))) [],SigD proj_2_1 (ForallT [PlainTV ty_24,PlainTV res_23] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT ty_24)) (VarT res_23))) (VarT res_23))),ValD (VarP proj_2_1) (NormalB (LamE [TupP [WildP,VarP x_25]] (VarE x_25))) [],SigD proj_3_0 (ForallT [PlainTV res_26,PlainTV ty_27,PlainTV ty_28] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT res_26)) (VarT ty_27)) (VarT ty_28))) (VarT res_26))),ValD (VarP proj_3_0) (NormalB (LamE [TupP [VarP x_29,WildP,WildP]] (VarE x_29))) [],SigD proj_3_1 (ForallT [PlainTV ty_31,PlainTV res_30,PlainTV ty_32] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_31)) (VarT res_30)) (VarT ty_32))) (VarT res_30))),ValD (VarP proj_3_1) (NormalB (LamE [TupP [WildP,VarP x_33,WildP]] (VarE x_33))) [],SigD proj_3_2 (ForallT [PlainTV ty_35,PlainTV ty_36,PlainTV res_34] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_35)) (VarT ty_36)) (VarT res_34))) (VarT res_34))),ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_37]] (VarE x_37))) [],SigD proj_4_0 (ForallT [PlainTV res_38,PlainTV ty_39,PlainTV ty_40,PlainTV ty_41] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT res_38)) (VarT ty_39)) (VarT ty_40)) (VarT ty_41))) (VarT res_38))),ValD (VarP proj_4_0) (NormalB (LamE [TupP [VarP x_42,WildP,WildP,WildP]] (VarE x_42))) [],SigD proj_4_1 (ForallT [PlainTV ty_44,PlainTV res_43,PlainTV ty_45,PlainTV ty_46] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_44)) (VarT res_43)) (VarT ty_45)) (VarT ty_46))) (VarT res_43))),ValD (VarP proj_4_1) (NormalB (LamE [TupP [WildP,VarP x_47,WildP,WildP]] (VarE x_47))) [],SigD proj_4_2 (ForallT [PlainTV ty_49,PlainTV ty_50,PlainTV res_48,PlainTV ty_51] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_49)) (VarT ty_50)) (VarT res_48)) (VarT ty_51))) (VarT res_48))),ValD (VarP proj_4_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_52,WildP]] (VarE x_52))) [],SigD proj_4_3 (ForallT [PlainTV ty_54,PlainTV ty_55,PlainTV ty_56,PlainTV res_53] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_54)) (VarT ty_55)) (VarT ty_56)) (VarT res_53))) (VarT res_53))),ValD (VarP proj_4_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_57]] (VarE x_57))) [],SigD proj_5_0 (ForallT [PlainTV res_58,PlainTV ty_59,PlainTV ty_60,PlainTV ty_61,PlainTV ty_62] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT res_58)) (VarT ty_59)) (VarT ty_60)) (VarT ty_61)) (VarT ty_62))) (VarT res_58))),ValD (VarP proj_5_0) (NormalB (LamE [TupP [VarP x_63,WildP,WildP,WildP,WildP]] (VarE x_63))) [],SigD proj_5_1 (ForallT [PlainTV ty_65,PlainTV res_64,PlainTV ty_66,PlainTV ty_67,PlainTV ty_68] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_65)) (VarT res_64)) (VarT ty_66)) (VarT ty_67)) (VarT ty_68))) (VarT res_64))),ValD (VarP proj_5_1) (NormalB (LamE [TupP [WildP,VarP x_69,WildP,WildP,WildP]] (VarE x_69))) [],SigD proj_5_2 (ForallT [PlainTV ty_71,PlainTV ty_72,PlainTV res_70,PlainTV ty_73,PlainTV ty_74] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_71)) (VarT ty_72)) (VarT res_70)) (VarT ty_73)) (VarT ty_74))) (VarT res_70))),ValD (VarP proj_5_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_75,WildP,WildP]] (VarE x_75))) [],SigD proj_5_3 (ForallT [PlainTV ty_77,PlainTV ty_78,PlainTV ty_79,PlainTV res_76,PlainTV ty_80] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_77)) (VarT ty_78)) (VarT ty_79)) (VarT res_76)) (VarT ty_80))) (VarT res_76))),ValD (VarP proj_5_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_81,WildP]] (VarE x_81))) [],SigD proj_5_4 (ForallT [PlainTV ty_83,PlainTV ty_84,PlainTV ty_85,PlainTV ty_86,PlainTV res_82] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_83)) (VarT ty_84)) (VarT ty_85)) (VarT ty_86)) (VarT res_82))) (VarT res_82))),ValD (VarP proj_5_4) (NormalB (LamE [TupP [WildP,WildP,WildP,WildP,VarP x_87]] (VarE x_87))) []]

