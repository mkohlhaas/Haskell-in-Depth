{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeclsGenerator where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.Socket (PortNumber)
import RemoteParser

remote ∷ QuasiQuoter
remote = QuasiQuoter
    { quoteExp = undefined,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = quoteFuncInfoDec
    }

quoteFuncInfoDec ∷ String → Q [Dec]
quoteFuncInfoDec quote = parseRemoteInterface quote >>= genClientStubs

-- >>> runQ $ parseRemoteInterface "ping :: RemotePing PingAnswer\necho :: String -> RemotePing String"
-- [FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))}]

-- >>> runQ $ parseRemoteInterface  "ping :: RemotePing PingAnswer\necho :: String -> RemotePing String" >>= genClientStubs
-- [SigD ping (AppT (ConT RemotePing) (ConT PingAnswer)),FunD ping [Clause [] (NormalB (InfixE (Just (LamE [VarP f_0] (AppE (VarE f_0) (ConE GHC.Tuple.())))) (VarE GHC.Base.$) (Just (AppE (VarE callRemote) (ListE [LitE (CharL 'p'),LitE (CharL 'i'),LitE (CharL 'n'),LitE (CharL 'g')]))))) []],SigD echo (AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))),FunD echo [Clause [] (NormalB (InfixE (Just (VarE GHC.Base.id)) (VarE GHC.Base.$) (Just (AppE (VarE callRemote) (ListE [LitE (CharL 'e'),LitE (CharL 'c'),LitE (CharL 'h'),LitE (CharL 'o')]))))) []]]

-- >>> runQ $ quoteFuncInfoDec "ping :: RemotePing PingAnswer\necho :: String -> RemotePing String"
-- [SigD ping (AppT (ConT RemotePing) (ConT PingAnswer)),FunD ping [Clause [] (NormalB (InfixE (Just (LamE [VarP f_2] (AppE (VarE f_2) (ConE GHC.Tuple.())))) (VarE GHC.Base.$) (Just (AppE (VarE callRemote) (ListE [LitE (CharL 'p'),LitE (CharL 'i'),LitE (CharL 'n'),LitE (CharL 'g')]))))) []],SigD echo (AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))),FunD echo [Clause [] (NormalB (InfixE (Just (VarE GHC.Base.id)) (VarE GHC.Base.$) (Just (AppE (VarE callRemote) (ListE [LitE (CharL 'e'),LitE (CharL 'c'),LitE (CharL 'h'),LitE (CharL 'o')]))))) []]]

genClientStubs ∷ [FuncInfo] → Q [Dec]
genClientStubs fis = concat <$> mapM (genClientStub "callRemote") fis

-- >>> runQ $ genClientStubs [FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))}]
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: PingAnswer :: Name
-- Data constructor not in scope: String :: Name
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: String :: Name

genClientStub ∷ String → FuncInfo → Q [Dec]
genClientStub callee FuncInfo {..} = do
  funcImpl ← funD funName [clause [] (normalB stubBody) []]
  pure [typeSig, funcImpl]
  where
    funName = mkName name
    typeSig = SigD funName ty
    stubBody = [|$(curryAll (arity ty)) $ $(dyn callee) name|]

-- >>> runQ $ genClientStub (FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))})
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: PingAnswer :: Name
-- Data constructor not in scope: String :: Name
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: String :: Name

-- >>> :type genClientStub (FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))})
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: PingAnswer :: Name
-- Data constructor not in scope: String :: Name
-- Data constructor not in scope: RemotePing :: Name
-- Data constructor not in scope: String :: Name

genServer ∷ [Name] → Q [Dec]
genServer names =
  [d|
    server ∷ String → PortNumber → IO ()
    server host port = serveRPC host port $(genRemoteTable names)
    |]

genRemoteTable ∷ [Name] → Q Exp
genRemoteTable names = mapM reifyFunc names >>= listE . map (genServerStub "runSerialized")

genServerStub ∷ String → FuncInfo → ExpQ
genServerStub callee FuncInfo {..} = [|(name, $(dyn callee) $ $(uncurryAll (arity ty)) $(dyn name))|]

reifyFunc ∷ Name → Q FuncInfo
reifyFunc nm = do
  VarI _ t Nothing ← reify nm
  pure $ FuncInfo (nameBase nm) t

arity ∷ Type → Int
arity (AppT (AppT ArrowT _) t) = arity t + 1
arity (ForallT _ _ t) = arity t
arity _ = 0

curryAll ∷ Int → Q Exp
curryAll 0 = [|\f → f ()|]
curryAll 1 = [|id|]
curryAll n
  | n > 1 = [|curry . $(curryAll (n -1))|]
  | otherwise = fail "curryAll argument can't be negative"

-- >>> runQ $ curryAll 3
-- InfixE (Just (VarE Data.Tuple.curry)) (VarE GHC.Base..) (Just (InfixE (Just (VarE Data.Tuple.curry)) (VarE GHC.Base..) (Just (VarE GHC.Base.id))))

-- >>> :type curryAll 3
-- curryAll 3 :: Q Exp

-- >>> :type $(curryAll 3)
-- $(curryAll 3) :: (((a, b1), b2) -> c) -> a -> b1 -> b2 -> c

uncurryAll ∷ Int → Q Exp
uncurryAll 0 = [|(const ∷ a → () → a)|]
uncurryAll 1 = [|id|]
uncurryAll n
  | n > 1 = [|uncurry . $(uncurryAll (n -1))|]
  | otherwise = fail "uncurryAll argument can't be negative"

-- >>> :type $(uncurryAll 3)
-- $(uncurryAll 3) :: (a -> b1 -> b2 -> c) -> ((a, b1), b2) -> c

-- >>> :type uncurryAll 3
-- uncurryAll 3 :: Q Exp

-- >>> runQ [e| Just x |]
-- AppE (ConE GHC.Maybe.Just) (UnboundVarE x)

-- >>> runQ [p| Just x |]
-- ConP GHC.Maybe.Just [VarP x_0]

add1 ∷ Q Exp
add1 = [| (+ 1) |]

-- >>> add1 1
-- Couldn't match expected type ‘t0 -> t’ with actual type ‘Q Exp’

-- >>> $add1 1
-- 2

-- >>> :type $add1 1
-- $add1 1 :: Num a => a

-- >>> runQ add1
-- InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))

-- >>> runQ add1
-- InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))

-- >>> runQ [| $add1 1 |]
-- AppE (InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))) (LitE (IntegerL 1))

-- >>> runQ [| add1 1 |]
-- AppE (VarE DeclsGenerator.add1) (LitE (IntegerL 1))

add2 ∷ Q Exp
add2 = [| $add1 . $add1 |]

-- >>> $add2 10
-- 12

-- >>> runQ add2
-- InfixE (Just (InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))) (VarE GHC.Base..) (Just (InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))))

-- >>> runQ [| $add2 10 |]
-- AppE (InfixE (Just (InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))) (VarE GHC.Base..) (Just (InfixE Nothing (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))))) (LitE (IntegerL 10))

