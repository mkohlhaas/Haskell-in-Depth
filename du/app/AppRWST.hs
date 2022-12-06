module AppRWST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.RWS (RWST (runRWST), evalRWST)

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

-- Reader = AppEnv
-- Writer =  files and e.g. its sizes, number of files, ... (the actual result of this tool) (grep for 'Myapp (')
-- State  = file size on entry and exit of a directory (grep for 'modify' and 'get')

runMyApp ∷ MyApp logEntry state a → AppConfig → state → IO [logEntry]
runMyApp app config = execRWSTW app (initialEnv config)

execRWSTW ∷ Monad m ⇒ RWST r w s m a → r → s → m w
execRWSTW m r s = do
  (_, _, w) ← runRWST m r s
  pure w
