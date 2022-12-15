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
curryAll 0 = [|\fn → fn ()|] -- function with no args
curryAll 1 = [|id|] --------- function with one arg
curryAll n ------------------ function with several args
  | n > 1 = [|curry . $(curryAll (n -1))|]
  | otherwise = fail "curryAll argument can't be negative"

-- ping ∷ RemotePing PingAnswer
-- ping = ((\f → f ()) $ callRemote "ping")

-- echo ∷ String → RemotePing String
-- echo = (id $ callRemote "echo")

-- echo1 ∷ String → String → RemotePing String
-- echo2 = ((curry . id) $ callRemote "echo2")
--
-- echo2 ∷ String → String → String → RemotePing String
-- echo3 = ((curry . (curry . id)) $ callRemote "echo3")

-- >>> :type id
-- id ∷ a → a

-- >>> :type curry
-- curry ∷ ((a, b) → c) → a → b → c

-- >>> :type curry . curry
-- curry . curry ∷ (((a, b1), b2) → c) → a → b1 → b2 → c

-- >>> :type $(curryAll 3)
-- $(curryAll 3) ∷ (((a, b1), b2) → c) → a → b1 → b2 → c

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
uncurryAll 0 = [|(const ∷ fn → () → fn)|]
uncurryAll 1 = [|id|]
uncurryAll n
  | n > 1 = [|uncurry . $(uncurryAll (n -1))|]
  | otherwise = fail "uncurryAll argument can't be negative"

-- >>> :type uncurry
-- uncurry ∷ (a → b → c) → (a, b) → c

-- >>> :type uncurry . uncurry
-- uncurry . uncurry ∷ (a → b1 → b2 → c) → ((a, b1), b2) → c

-- >>> :type uncurry . uncurry . id
-- uncurry . uncurry . id ∷ (a → b1 → b2 → c) → ((a, b1), b2) → c

-- >>> :type $(uncurryAll 3)
-- $(uncurryAll 3) ∷ (a → b1 → b2 → c) → ((a, b1), b2) → c

------------
-- Common --
------------

arity ∷ Type → Int
arity (AppT (AppT ArrowT _) t) = arity t + 1
arity (ForallT _ _ t) = arity t
arity _ = 0
