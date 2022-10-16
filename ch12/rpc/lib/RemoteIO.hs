-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RemoteIO where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Serialize hiding (get, put)
import Network.Connection
import Network.Socket (PortNumber)
import RpcCommon
import System.IO.Error (isEOFError)

-- processing decoding results: return decoded value or report an error within the monad stack
unEitherStaged ∷ DecodingStage → Either String a → RSIO st a
unEitherStaged stage = either (throwRemote . errMsg) pure
  where
    errMsg msg = "Decoding error (" <> show stage <> "): " <> msg

-- Main entry point for a client.
-- Open connection to server, run all operations, and close the connection.
runRemote ∷ RemoteState st ⇒ String → PortNumber → RSIO st a → IO a
runRemote host port computation = do
  conn ← remoteConnectTo host port
  res ← runRemoteConn conn computation
  liftIO $ connectionClose conn
  pure res

runRemoteConn ∷ RemoteState st ⇒ Connection → RSIO st a → IO a
runRemoteConn conn computation = runReaderT (evalStateT (runRem computation) initState) conn

sendRSIO ∷ Serialize a ⇒ a → RSIO st ()
sendRSIO msg = do
  conn ← ask
  liftIO $ connectionPut conn $ buildMsgEnvelope $ encode msg
  where
    buildMsgEnvelope payload = runPut $ do
      putWord64be (fromIntegral $ BS.length payload)
      putByteString payload

receiveRSIO ∷ Serialize a ⇒ RSIO st a
receiveRSIO =
  ask >>= \conn →
    recvExact conn msgSizeField
      >>= unEitherStaged Stage0 . runGet getWord64be
      >>= recvExact conn . fromIntegral
      >>= unEitherStaged Stage1 . decode
  where
    recvExact conn sz =
      catch
        (liftIO $ connectionGetExact conn sz)
        (\e → if isEOFError e then throwM ConnectionClosed else throwRemote (displayException e))

throwRemote ∷ String → RSIO st b
throwRemote errMsg = throwM $ RemoteException errMsg

remoteConnectTo ∷ String → PortNumber → IO Connection
remoteConnectTo host port = do
  connCtx ← initConnectionContext
  connectTo connCtx (ConnectionParams host port Nothing Nothing)
