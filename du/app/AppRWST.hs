module AppRWST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.RWS (RWST, evalRWST)

type MyApp logEntry state = RWST AppEnv [logEntry] state IO

runMyApp ∷ MyApp logEntry state a → AppConfig → state → IO (a, [logEntry])
runMyApp app config = evalRWST app (initialEnv config)
