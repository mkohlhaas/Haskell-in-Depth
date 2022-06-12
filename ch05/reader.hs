-- {-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader
  ( MonadReader (local),
    Reader,
    asks,
    runReader,
    when,
  )

newtype Config = Config
  { verbose :: Bool
  {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config {verbose = True {- ... -}}

work :: ConfigM ()
work = do
  -- ...
  doSomething

-- ...

doSomething :: ConfigM ()
doSomething = do
  -- ...
  doSomethingSpecial

-- ...

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask
  vrb <- asks verbose
  when vrb beVerbose

-- ...

beVerbose :: ConfigM ()
beVerbose = pure ()

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result
