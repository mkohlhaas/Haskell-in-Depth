{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE TupleSections #-}

module ServerUtils (genServer, serveRPC, runSerialized) where

import Control.Monad
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Serialize
import DeclsGenerator (genServer)
import Network.Connection
import Network.Simple.TCP
import Network.Socket (PortNumber)
import RemoteIO
import RpcCommon

-- runSerialized ∷ (Serialize a1, Serialize a2) ⇒ (a1 → RSIO st a2) → ByteString → RSIO st ByteString
runSerialized ∷ (Serialize a, Serialize b) ⇒ RemoteAction st a b → RemoteAction st ByteString ByteString
runSerialized action params = unEitherStaged Stage2 (decode params) >>= fmap encode . action

-- Main entry point for a server.
-- The `serve` function from the Network.Simple.TCP module creates a separate thread for every incoming connection.
-- We run the procRequests function in that thread.
serveRPC ∷ RemoteState st ⇒ HostName → PortNumber → RPCTable st → IO ()
serveRPC host portNum rpcTable = serve (Host host) (show portNum) procRequests
  where
    procRequests ∷ Show a ⇒ (Socket, a) → IO ()
    procRequests (connSock, sockAddr) = do
      logConnection "New connection" sockAddr
      initCtx ← initConnectionContext
      conn ← connectFromSocket initCtx connSock (ConnectionParams host portNum Nothing Nothing)
      catch
        (runRemoteConn conn $ forever (serveRequest rpcTable))
        (\(e ∷ RemoteException) → logConnection (displayException e) sockAddr)

    logConnection ∷ Show a ⇒ String → a → IO ()
    logConnection msg sockAddr =
      putStrLn $ "LOG: " <> show sockAddr <> " " <> msg

-- For every request the server looks up the RPCTable and calls the corresponding wrapped function.
serveRequest ∷ RPCTable st → RSIO st ()
serveRequest rpcTable = receiveRSIO >>= call >>= sendRSIO
  where
    call (operation, params) =
      case lookup operation rpcTable of
        Just func → func params
        Nothing → throwRemote $ "Unsupported operation (" <> operation <> ")"
