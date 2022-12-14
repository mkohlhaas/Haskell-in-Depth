{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeclsGenerator where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Network.Socket (PortNumber)
import RemoteParser (FuncInfo (..), parseRemoteInterface)

remote ∷ QuasiQuoter
remote =
  QuasiQuoter
    { quoteExp = undefined,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = quoteFuncInfoDec
    }

------------
-- Client --
------------

quoteFuncInfoDec ∷ String → Q [Dec]
quoteFuncInfoDec quote = parseRemoteInterface quote >>= genClientStubs

genClientStubs ∷ [FuncInfo] → Q [Dec]
genClientStubs fis = concat <$> mapM (genClientStub "callRemote") fis

genClientStub ∷ String → FuncInfo → Q [Dec]
genClientStub callee FuncInfo {..} = do
  funcImpl ← funD funName [clause [] (normalB stubBody) []]
  pure [typeSig, funcImpl]
  where
    funName = mkName name
    typeSig = SigD funName ty
    stubBody = [|$(curryAll (arity ty)) $ $(dyn callee) name|]

curryAll ∷ Int → Q Exp
curryAll 0 = [|\f → f ()|] -- function with no args
curryAll 1 = [|id|] --------- function with one arg
curryAll n ------------------ function with several args
  | n > 1 = [|curry . $(curryAll (n -1))|]
  | otherwise = fail "curryAll argument can't be negative"

------------
-- Server --
------------

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

uncurryAll ∷ Int → Q Exp
uncurryAll 0 = [|(const ∷ a → () → a)|] -- type signature won't hurt but not necessary
uncurryAll 1 = [|id|]
uncurryAll n
  | n > 1 = [|uncurry . $(uncurryAll (n -1))|]
  | otherwise = fail "uncurryAll argument can't be negative"

------------
-- Common --
------------

arity ∷ Type → Int
arity (AppT (AppT ArrowT _) t) = arity t + 1
arity (ForallT _ _ t) = arity t
arity _ = 0
