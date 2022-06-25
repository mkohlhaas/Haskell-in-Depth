{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State (MonadIO (liftIO), MonadState (get), modify)
import PingCommon (PingAnswer (..), RemotePing)
import ServerUtils (genServer, runSerialized, serveRPC)

ping :: RemotePing PingAnswer
ping = do
  modify (+ 1)
  n <- get
  liftIO $ putStrLn $ "Ping received/answered with " <> show n
  pure $ PingAnswer "OK" n

echo :: String -> RemotePing String
echo msg = do
  liftIO $ putStrLn $ "Echo message: " <> msg
  pure msg

genServer ['ping, 'echo]

main :: IO ()
main = server "localhost" 1500
