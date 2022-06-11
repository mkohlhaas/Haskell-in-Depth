module Data.Queue
  ( Queue,
    empty,
    isEmpty,
    front,
    enqueue,
    dequeue,
  )
where

import Data.Bool (Bool)
import Data.Deque hiding (empty, front, isEmpty)
import qualified Data.Deque as D (empty, front, isEmpty)
import Data.Maybe (Maybe)

newtype Queue a = Queue (Deque a)

empty :: Queue a
empty = Queue D.empty

isEmpty :: Queue a -> Bool
isEmpty (Queue d) = D.isEmpty d

front :: Queue a -> Maybe a
front (Queue d) = D.front d

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue d) = Queue (pushBack x d)

dequeue :: Queue a -> Queue a
dequeue (Queue d) = Queue (popFront d)
