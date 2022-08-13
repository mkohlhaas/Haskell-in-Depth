{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Logger (LoggingT, logDebug, runStdoutLoggingT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Text (pack)

popAndLog ∷ LoggingT (StateT [Int] IO) ()
popAndLog = do
  _ : xs ← lift get
  lift $ put xs
  $logDebug ("***" <> pack (show xs) <> "***")

logStateEx ∷ LoggingT (StateT [Int] IO) Int
logStateEx = do
  popAndLog
  popAndLog
  pure 5

main ∷ IO ()
main = runStateT (runStdoutLoggingT logStateEx) [1, 2, 3] >>= print
