import Control.Concurrent
import Control.Exception

oneSec ∷ Int
oneSec = 1000000

acquire ∷ IO ()
acquire = do
  putStrLn "Start resource acquisition"
  threadDelay oneSec
  putStrLn "Resource is acquired"

use ∷ IO ()
use = do
  putStrLn "Begin using the resource"
  threadDelay (2 * oneSec)
  putStrLn "End using the resource"

release ∷ IO ()
release = do
  putStrLn "Start releasing the resource"
  threadDelay oneSec
  putStrLn "Resource is released"

workWithResource ∷ IO ()
workWithResource = do
  acquire
  use `onException` release
  release

workWithResourceSafe ∷ IO ()
workWithResourceSafe = uninterruptibleMask $ \restore → do
  acquire
  restore use `onException` release
  release

experiment ∷ Int → IO () → IO ()
experiment timeout action = do
  thr ← forkIO action
  threadDelay timeout
  killThread thr
  threadDelay (2 * oneSec)

main ∷ IO ()
main = do
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `acquire`      --"
  putStrLn "--------------------------------------------------"
  experiment (oneSec `div` 2) workWithResource

  putStrLn ""
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `use`          --"
  putStrLn "--------------------------------------------------"
  experiment (oneSec + oneSec `div` 2) workWithResource

  putStrLn ""
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `release`      --"
  putStrLn "--------------------------------------------------"
  experiment (3 * oneSec + oneSec `div` 2) workWithResource

  putStrLn ""
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `acquire`/safe --"
  putStrLn "--------------------------------------------------"
  experiment (oneSec `div` 2) workWithResourceSafe

  putStrLn ""
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `use`/safe     --"
  putStrLn "--------------------------------------------------"
  experiment (oneSec + oneSec `div` 2) workWithResourceSafe

  putStrLn ""
  putStrLn "--------------------------------------------------"
  putStrLn "-- Asynchronous exception during `release`/safe --"
  putStrLn "--------------------------------------------------"
  experiment (3 * oneSec + oneSec `div` 2) workWithResourceSafe
