{-# LANGUAGE ScopedTypeVariables #-}

module ServerUtils (genServer, serveRPC, runSerialized) where

import Control.Monad (forever)
import Control.Monad.Catch (Exception (displayException), MonadCatch (catch))
import Data.ByteString (ByteString)
import Data.Serialize (Serialize, decode, encode)
import DeclsGenerator (genServer)
import Network.Connection (ConnectionParams (ConnectionParams), connectFromSocket, initConnectionContext)
import Network.Simple.TCP (HostName, HostPreference (Host), Socket, serve)
import Network.Socket (HostName, PortNumber, SockAddr, Socket)
import RemoteIO (receiveRSIO, runRemoteConn, sendRSIO, throwRemote, unEitherStaged)
import RpcCommon (DecodingStage (Stage2), Operation, RPCTable, RSIO, RemoteAction, RemoteException, RemoteState)

-- decode a and encode b
-- This is basically lifting a generic RemoteAction to a RemoteAction with ByteStrings.
runSerialized ∷ (Serialize a, Serialize b) ⇒ RemoteAction st a b → RemoteAction st ByteString ByteString
runSerialized action params = unEitherStaged Stage2 (decode params) >>= fmap encode . action

-- For every request the server looks up the RPCTable and calls the corresponding wrapped function.
serveRequest ∷ ∀ st. RPCTable st → RSIO st ()
serveRequest rpcTable = receiveRSIO >>= call >>= sendRSIO
  where
    call ∷ (Operation, ByteString) → RSIO st ByteString
    call (operation, params) =
      case lookup operation rpcTable of
        Just func → func params
        Nothing → throwRemote $ "Unsupported operation (" <> operation <> ")"

-- Main entry point for a server.
-- The `serve` function from the Network.Simple.TCP module creates a separate thread for every incoming connection.
-- We run the procRequests function in that thread.
serveRPC ∷ RemoteState st ⇒ HostName → PortNumber → RPCTable st → IO ()
serveRPC host portNum rpcTable = serve (Host host) (show portNum) procRequests
  where
    procRequests ∷ (Socket, SockAddr) → IO ()
    procRequests (connSock, sockAddr) = do
      logConnection "New connection" sockAddr
      connextCtx ← initConnectionContext
      conn ← connectFromSocket connextCtx connSock (ConnectionParams host portNum Nothing Nothing)
      catch
        (runRemoteConn conn $ forever (serveRequest rpcTable))
        (\(e ∷ RemoteException) → logConnection (displayException e) sockAddr)
    logConnection ∷ String → SockAddr → IO ()
    logConnection msg sockAddr =
      putStrLn $ "LOG: " <> show sockAddr <> " " <> msg
