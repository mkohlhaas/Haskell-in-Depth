{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

import ClientUtils
import Control.Monad
import Control.Monad.Trans
import PingCommon

[remote|
ping :: RemotePing PingAnswer
echo :: String -> RemotePing String
echo2 :: String -> String -> RemotePing String
echo3 :: String -> String -> String -> RemotePing String
 |]

-- This will be generated by TH:
-- ping ∷ RemotePing PingAnswer
-- ping = ((\ f_abbv → f_abbv ()) $ callRemote "ping")

-- ping = ((\ f_abbv → f_abbv ()) $ callRemote "ping")
-- the same
-- ping = callRemote "ping" ()

-- echo ∷ String → RemotePing String
-- echo = (id $ callRemote "echo")

-- echo = (id $ callRemote "echo")
-- echo = callRemote "echo"


-- ping :: RemotePing PingAnswer
-- ping = ((\f -> f ()) $ callRemote "ping")
--
-- echo :: String -> RemotePing String
-- echo = (id $ callRemote "echo")
--
-- echo2 :: String -> String -> RemotePing String
-- echo2 = ((curry . id) $ callRemote "echo2")
--
-- echo3 :: String -> String -> String -> RemotePing String
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
