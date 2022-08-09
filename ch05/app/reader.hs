{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader (MonadReader (local), Reader, ask, asks, runReader, when)

data Config = Config {verbose ∷ Bool, debug ∷ Bool {- more config params -}}

type ConfigM = Reader Config

getConfiguration ∷ IO Config
getConfiguration = pure Config {verbose = True, debug = False {- more config params -}}

work ∷ ConfigM ()
work = do
  -- ...
  doSomething

doSomething ∷ ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial
  doSomethingSpecialSilently

doSomethingSpecial ∷ ConfigM ()
doSomethingSpecial = do
  -- ...
  verbose ← asks verbose
  Config {verbose} ← ask -- the same
  when verbose beVerbose

beVerbose ∷ ConfigM ()
beVerbose = pure ()

silent ∷ Config → Config
silent config = config {verbose = False}

doSomethingSpecialSilently ∷ ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

main ∷ IO ()
main = runReader work <$> getConfiguration
