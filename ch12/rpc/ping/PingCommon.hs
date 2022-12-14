{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module PingCommon where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import RpcCommon (RSIO, RemoteState (..))

-- >>> :info Serialize
-- type Serialize ∷ * → Constraint
-- class Serialize t where
--   put ∷ Putter t
--   default put ∷ (Generic t, GSerializePut (Rep t)) ⇒ Putter t
--   get ∷ Get t
--   default get ∷ (Generic t, GSerializeGet (Rep t)) ⇒ Get t

-- State for one client-server session.
instance RemoteState Integer where
  initState ∷ Integer
  initState = 0

-- main application monad to work with
-- RSIO = State:         Integer
--        Reader:        Connection
--        base monad:    IO
--
--                     running number (State)
--          result of       |
--       the remote call    |
--              |           |
type RemotePing a = RSIO Integer a

-- e.g.
--             running number/number of pings
--                 |
-- PingAnswer "Ok" 2
data PingAnswer = PingAnswer !String !Integer
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)
