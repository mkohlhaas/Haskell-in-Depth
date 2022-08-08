{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRTWTST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT))

newtype MyApp logEntry state a = MyApp {runApp ∷ ReaderT AppEnv (WriterT [logEntry] (StateT state IO)) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadWriter [logEntry], MonadState state)

runMyApp ∷ MyApp logEntry state a → AppConfig → state → IO (a, [logEntry])
runMyApp app config = evalStateT (runWriterT (runReaderT (runApp app) (initialEnv config)))
