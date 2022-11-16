{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}

----------------------------------------------------------------------------------------------------------------
--                                 Bringing Values to the Type Level                                          --
----------------------------------------------------------------------------------------------------------------

-- To bring values to type level, we introduce a whole bunch of types with only one value in each of them.
-- Such types, just as sets with a single element, are called singleton types or simply singletons.
-- If every such type has only one value, then we can easily go between term level and type level.
-- If we have a value, then we know its type (as always in Haskell).
-- With singletons, if we have a type, we can be 100% sure about the value also, because only one value exists.

-- With the DataKinds extension, we get types Opened and Closed right away.
-- Note that the promoted types have no values.
data DoorState = Opened | Closed
  deriving (Show)

-- >>> :kind Opened
-- Opened ∷ DoorState

-- >>> :kind 'Opened
-- 'Opened ∷ DoorState

-- Let's define two singleton values - SClosed and SOpened - for Opened and Closed as GADTs.
-- One value, one type.
data SDoorState (s ∷ DoorState) where
  SClosed ∷ SDoorState Closed
  SOpened ∷ SDoorState Opened

-- The SDoorState Closed type is a singleton. There exists only one value of this type, namely, SClosed.
-- SDoorState Opened is another singleton.

-- >>> :type SOpened
-- SOpened ∷ SDoorState 'Opened

-- >>> :type SClosed
-- SClosed ∷ SDoorState 'Closed

-- The SClosed and SOpened values are EXPLICIT singletons.
-- Sometimes it's more convenient to work with them IMPLICITLY via a type class.
class SDoorStateI (s ∷ DoorState) where
  sDoorState ∷ SDoorState s

instance SDoorStateI Opened where
  sDoorState ∷ SDoorState 'Opened
  sDoorState = SOpened

instance SDoorStateI Closed where
  sDoorState ∷ SDoorState 'Closed
  sDoorState = SClosed

-- We build the door with the MkDoor data constructor.
-- Note that it has no arguments.
-- Instead, it introduces the SDoorStateI constraint.
-- Depending on the door type we actually build, `Door Closed` or `Door Opened`, GHC adds the corresponding instance!

-- Suppose we have the MkDoor value. How do we get its state? Is it opened or closed?
-- Here is how singletons come into play.
-- We should consult the corresponding SDoorStateI instance, get a singleton, and pattern match over it.

-- Having an SDoorStateI constraint, ...
data Door (s ∷ DoorState) where
  MkDoor ∷ SDoorStateI s ⇒ Door s

-- ... we can use the sDoorState instance method to acquire an explicit singleton and then pattern match over it.
-- `doorState` goes from type to value.
doorState ∷ ∀ s. Door s → DoorState
doorState MkDoor =
  case sDoorState ∷ SDoorState s of -- sDoorState method; type annotation necessary
    SOpened → Opened ----------------- pattern matching
    SClosed → Closed ----------------- pattern matching

-- usage of doorState
instance Show (Door s) where
  show ∷ Door s → String
  show d = "Door " <> show (doorState d) <> "."

-- Sometimes, we don't need to refer to singletons.
-- E.g. in `open` and `close` it's enough to control the types!
-- Behind the scenes GHC adds the corresponding SDoorStateI instances to MkDoor.

-- `open` can only go from a closed to an opened door.
open ∷ Door Closed → Door Opened
open _ = MkDoor

-- `close` can only go from an opened to a closed door.
close ∷ Door Opened → Door Closed
close _ = MkDoor

-- We can't write a function that returns `Door s` for some particular `s` depending on user input.

-- Therefore we hide `s` inside an existential.
data SomeDoor where
  SomeDoor ∷ Door s → SomeDoor

deriving instance Show SomeDoor

-- >>> show (MkDoor ∷ Door Opened)
-- "Door Opened."

-- >>> show $ SomeDoor (MkDoor ∷ Door Opened)
-- "SomeDoor Door Opened."

-- With SomeDoor it's easy to parse user input and build a door.
-- Instead of `Door s` - which can't be returned based on user input - we return (Maybe) SomeDoor.
parseDoor ∷ String → Maybe SomeDoor
parseDoor "Opened" = Just $ SomeDoor (MkDoor ∷ Door Opened) -- explicit type annotation leads GHC to add the corresponding SDoorStateI instances
parseDoor "Closed" = Just $ SomeDoor (MkDoor ∷ Door Closed)
parseDoor _ = Nothing

-- GHC error: "Expecting one more argument to ‘Door’. Expected a type, but ‘Door’ has kind ‘DoorState -> *’"
-- parseDoor ∷ String → Maybe Door
-- parseDoor = undefined

-- We don't know the resulting type in advance, so we have to hide it inside SomeDoor.
switchState ∷ ∀ s. Door s → SomeDoor
switchState door@MkDoor =
  case sDoorState ∷ SDoorState s of
    SOpened → SomeDoor $ close door
    SClosed → SomeDoor $ open door

-- Open an already opened door, or closing an already closed door is not possible.
-- switchState ∷ ∀ s. Door s → SomeDoor
-- switchState door@MkDoor =
--   case sDoorState ∷ SDoorState s of
--     SOpened → SomeDoor $ open door
--     SClosed → SomeDoor $ close door

switchSome ∷ SomeDoor → SomeDoor
switchSome (SomeDoor d) = switchState d

doorState' ∷ ∀ s. SomeDoor → DoorState
doorState' (SomeDoor d) = doorState d

-- >>> doorState' <$> parseDoor "Opened"
-- Just Opened

-- >>> doorState' <$> parseDoor "Closed"
-- Just Closed

-- >>> doorState' <$> parseDoor "Half-Opened"
-- Nothing

test ∷ String → IO ()
test d =
  case parseDoor d of
    Just door → do
      putStrLn $ "Given: " <> show door
      putStrLn $ "Switched: " <> show (switchSome door)
    Nothing → putStrLn "Incorrect argument"

main ∷ IO ()
main = do
  test "Opened"
  test "Closed"
