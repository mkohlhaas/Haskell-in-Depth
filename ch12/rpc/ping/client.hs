{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

import ClientUtils (callRemote, remote, runRemote)
import Control.Monad (replicateM_)
import Control.Monad.Trans (MonadIO (liftIO))
import PingCommon (PingAnswer, RemotePing)

[remote|
ping :: RemotePing PingAnswer
echo :: String -> RemotePing String
echo1 :: String -> String -> RemotePing String
echo2 :: String -> String -> String -> RemotePing String
 |]

-- ⇒
-- ping ∷ RemotePing PingAnswer
-- ping = ((\f → f ()) $ callRemote "ping")

-- echo ∷ String → RemotePing String
-- echo = (id $ callRemote "echo")

-- echo1 ∷ String → String → RemotePing String
-- echo2 = ((curry . id) $ callRemote "echo2")
--
-- echo2 ∷ String → String → String → RemotePing String
-- echo3 = ((curry . (curry . id)) $ callRemote "echo3")

example ∷ Int → RemotePing ()
example n = do
  echo "Hello from client" >>= prt
  replicateM_ n (ping >>= prt)
  echo "Bye from client" >>= prt
  where
    prt ∷ Show a ⇒ a → RemotePing ()
    prt = liftIO . print

main ∷ IO ()
main = runRemote "localhost" 1500 (example 3)
