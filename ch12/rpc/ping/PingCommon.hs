{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module PingCommon where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import RpcCommon (RSIO, RemoteState (..))

instance RemoteState Integer where
  initState = 0

type RemotePing a = RSIO Integer a

data PingAnswer = PingAnswer String Integer
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)
