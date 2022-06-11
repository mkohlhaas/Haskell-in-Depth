import Data.Deque as D
import Data.List
import Data.Stack as S
import System.TimeIt

sumAll s view remove = sum $ unfoldr iter s
  where
    iter s = view s >>= \x -> Just (x, remove s)

fill n insert s = foldl' (flip insert) s [1 .. n]

main = do
  let n = 10 ^ 6
  timeItNamed "Stack" $
    print $ sumAll (fill n push S.empty) top pop
  timeItNamed "Deque" $
    print $ sumAll (fill n pushFront D.empty) front popFront
