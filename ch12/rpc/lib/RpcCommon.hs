{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module RpcCommon where

import Control.Monad.Catch (Exception, MonadCatch, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT (StateT))
import Data.ByteString (ByteString)
import Network.Connection (Connection)

msgSizeField ∷ Int
msgSizeField = 8 -- in bytes

data RemoteException = ConnectionClosed | RemoteException !String

instance Show RemoteException where
  show ∷ RemoteException → String
  show ConnectionClosed = "Connection closed"
  show (RemoteException msg) = "Remote Exception: " <> msg

-- >>> :info Exception
-- type Exception :: * -> Constraint
-- class (Typeable e, Show e) => Exception e where
--   toException :: e -> SomeException
--   fromException :: SomeException -> Maybe e
--   displayException :: e -> String

instance Exception RemoteException

class RemoteState a where
  initState ∷ a

instance RemoteState () where
  initState ∷ ()
  initState = ()

-- application stack
newtype RSIO st a = RSIO {runRem ∷ StateT st (ReaderT Connection IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Connection, MonadState st, MonadThrow, MonadCatch)

type Operation = String

-- a = input params of client
-- b = output of server
-- Basically function `a → b` embedded in a monadic context.
type RemoteAction st a b = a → RSIO st b

type RPCTable st = [(Operation, RemoteAction st ByteString ByteString)]

-- Stage0: Decode the first field of an envelope (the size of the payload).
-- Stage1: Decode the second field of an envelope to a ByteString.
-- Stage2: Decode the ByteString to a value we expect.
--         Server: result of a function call
--         Client: tuple with all the parameters
data DecodingStage = Stage0 | Stage1 | Stage2
  deriving (Show)
