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

runSerialized :: (Serialize a, Serialize b) => RemoteAction st a b -> RemoteAction st ByteString ByteString
runSerialized action params = unEitherStaged Stage2 (decode params) >>= fmap encode . action

serveRPC :: RemoteState st => HostName -> PortNumber -> RPCTable st -> IO ()
serveRPC host portNum rpcTable = serve (Host host) (show portNum) procRequests
  where
    connParams = ConnectionParams host portNum Nothing Nothing

    procRequests (connSock, sockAddr) = do
      logConnection "New connection" sockAddr
      initCtx <- initConnectionContext
      conn <- connectFromSocket initCtx connSock connParams
      catch (runRemoteConn conn $ forever $ serveRequest rpcTable) (\(e :: RemoteException) -> logConnection (displayException e) sockAddr)

    logConnection msg sockAddr =
      putStrLn $ "LOG: " <> show sockAddr <> " " <> msg

serveRequest :: RPCTable st -> RSIO st ()
serveRequest rpcTable = receiveRSIO >>= call >>= sendRSIO
  where
    call (operation, params) =
      case lookup operation rpcTable of
        Nothing ->
          throwRemote $
            "Unsupported operation (" <> operation <> ")"
        Just func -> func params
