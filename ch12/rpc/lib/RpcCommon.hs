{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RpcCommon where

import Control.Monad.Catch (Exception, MonadCatch, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT (StateT))
import Data.ByteString (ByteString)
import Network.Connection (Connection)

msgSizeField :: Int
msgSizeField = 8 -- in bytes

data RemoteException = ConnectionClosed | RemoteException String

instance Show RemoteException where
  show ConnectionClosed = "Connection closed"
  show (RemoteException msg) = "Remote Exception: " <> msg

instance Exception RemoteException

class RemoteState a where
  initState :: a

instance RemoteState () where
  initState = ()

newtype RSIO st a = RSIO {runRem :: StateT st (ReaderT Connection IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection, MonadState st, MonadThrow, MonadCatch)

type Operation = String

type RemoteAction st a b = a -> RSIO st b

type RPCTable st = [(Operation, RemoteAction st ByteString ByteString)]

data DecodeStages = Stage0 | Stage1 | Stage2
  deriving (Show)
