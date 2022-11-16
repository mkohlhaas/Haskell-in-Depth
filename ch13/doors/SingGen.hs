{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Data.Singletons.TH

-- NEW
$(singletons [d|data DoorState = Opened | Closed deriving (Show)|])

-- We declare that we want to turn Opened and Closed into singletons,
-- This TH splice generates the following:
-- - `SDoorState s` type with values SOpened and SClosed. The type name is an alias to Sing.
-- - `SingI s` type class instances for SOpened and SClosed.
--   This type class provides the `sing` method (analogous to the sDoorState method from the previous version).
-- - The `fromSing` method returns the original DoorState value when given the corresponding singleton.

-- NEW
data Door (s ∷ DoorState) where
  MkDoor ∷ SingI s ⇒ Door s

-- NEW
doorState ∷ ∀ s. Door s → DoorState
doorState MkDoor = fromSing (sing ∷ SDoorState s)

-- the same
instance Show (Door s) where
  show d = "Door " <> show (doorState d) <> "."

-- the same
open ∷ Door Closed → Door Opened
open _ = MkDoor

-- the same
close ∷ Door Opened → Door Closed
close _ = MkDoor

-- the same
data SomeDoor where
  SomeDoor ∷ Door s → SomeDoor

-- the same
deriving instance Show SomeDoor

-- the same
parseDoor ∷ String → Maybe SomeDoor
parseDoor "Opened" = Just $ SomeDoor (MkDoor ∷ Door Opened)
parseDoor "Closed" = Just $ SomeDoor (MkDoor ∷ Door Closed)
parseDoor _ = Nothing

-- NEW (using `sing` method)
switchState ∷ ∀ s. Door s → SomeDoor
switchState door@MkDoor =
  case sing ∷ SDoorState s of
    SOpened → SomeDoor $ close door
    SClosed → SomeDoor $ open door

-- the same
switchSome ∷ SomeDoor → SomeDoor
switchSome (SomeDoor d) = switchState d

-- the same
doorState' ∷ ∀ s. SomeDoor → DoorState
doorState' (SomeDoor d) = doorState d

-- >>> doorState' <$> parseDoor "Opened"
-- Just Opened

-- >>> doorState' <$> parseDoor "Closed"
-- Just Closed

-- >>> doorState' <$> parseDoor "Half-Opened"
-- Nothing

-- the same
test ∷ String → IO ()
test d =
  case parseDoor d of
    Just door → do
      putStrLn $ "Given: " <> show door
      putStrLn $ "Switched: " <> show (switchSome door)
    Nothing → putStrLn "Incorrect argument"

-- the same
main ∷ IO ()
main = do
  test "Opened"
  test "Closed"
