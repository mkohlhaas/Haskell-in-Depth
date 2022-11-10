module ClientUtils
  ( module RpcCommon,
    module RemoteIO,
    remote,
    callRemote,
  )
where

import Data.Serialize (Serialize, decode, encode)
import DeclsGenerator (remote)
import RemoteIO
import RpcCommon

-- encode ⇒ send ⇒ receive ⇒ decode
callRemote ∷ (Serialize a, Serialize b) ⇒ Operation → RemoteAction st a b
callRemote operation params = do
  sendRSIO (operation, encode params)
  answer ← receiveRSIO
  unEitherStaged Stage2 (decode answer)
