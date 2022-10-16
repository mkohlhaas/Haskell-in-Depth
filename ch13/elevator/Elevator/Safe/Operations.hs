{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Elevator.Safe.Operations where

import Control.Monad.Trans
import Data.Singletons.TH
import Data.Type.Nat
import qualified Elevator.LowLevel as LL
import Elevator.Safe.Floor

$( singletons
     [d|
       data Door = Opened | Closed
         deriving (Eq, Show)
       |]
 )

data Elevator (mx ∷ Nat) (cur ∷ Nat) (door ∷ Door) where
  MkElevator ∷ SingI door ⇒ Floor mx cur → Elevator mx cur door

currentFloor ∷ Elevator mx cur door → Floor mx cur
currentFloor (MkElevator fl) = fl

currentDoor ∷ ∀ mx cur door. Elevator mx cur door → Door
currentDoor (MkElevator _) = fromSing (sing ∷ Sing door)

instance Show (Elevator mx cur door) where
  show el =
    "Elevator {current = " <> show (currentFloor el)
      <> ", door = "
      <> show (currentDoor el)
      <> "}"

up ∷
  (BelowTop mx cur, MonadIO m) ⇒
  Elevator mx cur Closed →
  m (Elevator mx (S cur) Closed)
up (MkElevator fl) = do
  liftIO LL.up
  pure (MkElevator $ next fl)

down ∷ MonadIO m ⇒ Elevator mx (S cur) Closed → m (Elevator mx cur Closed)
down (MkElevator fl) = do
  liftIO LL.down
  pure $ MkElevator $ prev fl

open ∷
  MonadIO m ⇒
  Floor mx cur →
  Elevator mx cur Closed →
  m (Elevator mx cur Opened)
open _ (MkElevator fl) = do
  liftIO LL.open
  pure (MkElevator fl)

close ∷
  MonadIO m ⇒
  Floor mx cur →
  Elevator mx cur Opened →
  m (Elevator mx cur Closed)
close _ (MkElevator fl) = do
  liftIO LL.close
  pure (MkElevator fl)

ensureClosed ∷
  ∀ mx cur door m.
  MonadIO m ⇒
  Elevator mx cur door →
  m (Elevator mx cur Closed)
ensureClosed el@(MkElevator fl) =
  case sing ∷ Sing door of
    SClosed → pure el
    SOpened → close fl el

ensureOpenedAt ∷
  ∀ mx cur door m.
  MonadIO m ⇒
  Floor mx cur →
  Elevator mx cur door →
  m (Elevator mx cur Opened)
ensureOpenedAt fl el@(MkElevator _) =
  case sing ∷ Sing door of
    SOpened → pure el
    SClosed → open fl el
