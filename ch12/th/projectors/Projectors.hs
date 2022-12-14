{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Projectors where

import Language.Haskell.TH
import Data.List

-- Let's see what GHC will generate!
-- >>> runQ [| \(x, _, _) → x |]
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
 | n > 1 && 0 ⇐ k && k < n = do
    x ← newName "x"
    let mkPat j
          | j == k = VarP x
          | otherwise = WildP
    pure $ LamE [TupP $ map mkPat [0..n-1]] (VarE x)
 | n ⇐ 1 = fail "Wrong number of tuple elements (must be > 1)"
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
-- So they are composed better with the $(...) splice operator that expects `Q a` values inside it, e.g.
-- TupP ∷ [Pat]  → Pat
-- tupP ∷ [PatQ] → PatQ

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
-- >>> runQ [t| (a, b) → a |]
-- Not in scope: type variable ‘a’
-- Not in scope: type variable ‘b’
-- Not in scope: type variable ‘a’

-- We need the `∀` keyword.
-- >>> runQ [t| ∀ a b. (a, b) → a |]
-- ForallT [PlainTV a_11,PlainTV b_12] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_11)) (VarT b_12))) (VarT a_11))

-- >>> runQ [t| ∀ a b c. (a, b, c) → a |]
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
    mkTuple tys = pure $ foldl' addApp (TupleT n) tys
    addApp ∷ Type → Name → Type
    addApp acc_ty ty = AppT acc_ty (VarT ty)

-- >>> runQ funTy
-- ForallT [PlainTV ty_17,PlainTV ty_18,PlainTV res_16] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_17)) (VarT ty_18)) (VarT res_16))) (VarT res_16))

-- >>> runQ $ mkProjType 3 1
-- SigD proj_3_1 (ForallT [PlainTV ty_1,PlainTV res_0,PlainTV ty_2] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_1)) (VarT res_0)) (VarT ty_2))) (VarT res_0)))

mkProjectors ∷ [Int] → Q [Dec]
mkProjectors = fmap concat . mapM projectors
  where
    projectors ∷ Int → Q [Dec]
    projectors n = concat <$> mapM (mkProj n) [0 .. n -1]
    mkProj ∷ Int → Int → Q [Dec]
    mkProj n k = (:) <$> mkProjType n k <*> mkProjDec n k

-- >>> runQ $ mkProjectors [2 .. 5]
-- [SigD proj_2_0 (ForallT [PlainTV res_3,PlainTV ty_4] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT res_3)) (VarT ty_4))) (VarT res_3))),ValD (VarP proj_2_0) (NormalB (LamE [TupP [VarP x_5,WildP]] (VarE x_5))) [],SigD proj_2_1 (ForallT [PlainTV ty_7,PlainTV res_6] [] (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT ty_7)) (VarT res_6))) (VarT res_6))),ValD (VarP proj_2_1) (NormalB (LamE [TupP [WildP,VarP x_8]] (VarE x_8))) [],SigD proj_3_0 (ForallT [PlainTV res_9,PlainTV ty_10,PlainTV ty_11] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT res_9)) (VarT ty_10)) (VarT ty_11))) (VarT res_9))),ValD (VarP proj_3_0) (NormalB (LamE [TupP [VarP x_12,WildP,WildP]] (VarE x_12))) [],SigD proj_3_1 (ForallT [PlainTV ty_14,PlainTV res_13,PlainTV ty_15] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_14)) (VarT res_13)) (VarT ty_15))) (VarT res_13))),ValD (VarP proj_3_1) (NormalB (LamE [TupP [WildP,VarP x_16,WildP]] (VarE x_16))) [],SigD proj_3_2 (ForallT [PlainTV ty_18,PlainTV ty_19,PlainTV res_17] [] (AppT (AppT ArrowT (AppT (AppT (AppT (TupleT 3) (VarT ty_18)) (VarT ty_19)) (VarT res_17))) (VarT res_17))),ValD (VarP proj_3_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_20]] (VarE x_20))) [],SigD proj_4_0 (ForallT [PlainTV res_21,PlainTV ty_22,PlainTV ty_23,PlainTV ty_24] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT res_21)) (VarT ty_22)) (VarT ty_23)) (VarT ty_24))) (VarT res_21))),ValD (VarP proj_4_0) (NormalB (LamE [TupP [VarP x_25,WildP,WildP,WildP]] (VarE x_25))) [],SigD proj_4_1 (ForallT [PlainTV ty_27,PlainTV res_26,PlainTV ty_28,PlainTV ty_29] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_27)) (VarT res_26)) (VarT ty_28)) (VarT ty_29))) (VarT res_26))),ValD (VarP proj_4_1) (NormalB (LamE [TupP [WildP,VarP x_30,WildP,WildP]] (VarE x_30))) [],SigD proj_4_2 (ForallT [PlainTV ty_32,PlainTV ty_33,PlainTV res_31,PlainTV ty_34] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_32)) (VarT ty_33)) (VarT res_31)) (VarT ty_34))) (VarT res_31))),ValD (VarP proj_4_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_35,WildP]] (VarE x_35))) [],SigD proj_4_3 (ForallT [PlainTV ty_37,PlainTV ty_38,PlainTV ty_39,PlainTV res_36] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (TupleT 4) (VarT ty_37)) (VarT ty_38)) (VarT ty_39)) (VarT res_36))) (VarT res_36))),ValD (VarP proj_4_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_40]] (VarE x_40))) [],SigD proj_5_0 (ForallT [PlainTV res_41,PlainTV ty_42,PlainTV ty_43,PlainTV ty_44,PlainTV ty_45] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT res_41)) (VarT ty_42)) (VarT ty_43)) (VarT ty_44)) (VarT ty_45))) (VarT res_41))),ValD (VarP proj_5_0) (NormalB (LamE [TupP [VarP x_46,WildP,WildP,WildP,WildP]] (VarE x_46))) [],SigD proj_5_1 (ForallT [PlainTV ty_48,PlainTV res_47,PlainTV ty_49,PlainTV ty_50,PlainTV ty_51] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_48)) (VarT res_47)) (VarT ty_49)) (VarT ty_50)) (VarT ty_51))) (VarT res_47))),ValD (VarP proj_5_1) (NormalB (LamE [TupP [WildP,VarP x_52,WildP,WildP,WildP]] (VarE x_52))) [],SigD proj_5_2 (ForallT [PlainTV ty_54,PlainTV ty_55,PlainTV res_53,PlainTV ty_56,PlainTV ty_57] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_54)) (VarT ty_55)) (VarT res_53)) (VarT ty_56)) (VarT ty_57))) (VarT res_53))),ValD (VarP proj_5_2) (NormalB (LamE [TupP [WildP,WildP,VarP x_58,WildP,WildP]] (VarE x_58))) [],SigD proj_5_3 (ForallT [PlainTV ty_60,PlainTV ty_61,PlainTV ty_62,PlainTV res_59,PlainTV ty_63] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_60)) (VarT ty_61)) (VarT ty_62)) (VarT res_59)) (VarT ty_63))) (VarT res_59))),ValD (VarP proj_5_3) (NormalB (LamE [TupP [WildP,WildP,WildP,VarP x_64,WildP]] (VarE x_64))) [],SigD proj_5_4 (ForallT [PlainTV ty_66,PlainTV ty_67,PlainTV ty_68,PlainTV ty_69,PlainTV res_65] [] (AppT (AppT ArrowT (AppT (AppT (AppT (AppT (AppT (TupleT 5) (VarT ty_66)) (VarT ty_67)) (VarT ty_68)) (VarT ty_69)) (VarT res_65))) (VarT res_65))),ValD (VarP proj_5_4) (NormalB (LamE [TupP [WildP,WildP,WildP,WildP,VarP x_70]] (VarE x_70))) []]

