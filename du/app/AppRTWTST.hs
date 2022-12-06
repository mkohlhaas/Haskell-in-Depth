{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppRTWTST where

import AppTypes (AppConfig, AppEnv, initialEnv)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Monad.Writer (MonadWriter, WriterT (runWriterT), execWriterT)

-- Reader = AppEnv
-- Writer = all the files and its sizes (the actual result of this tool)
-- State  = file size on entry and exit of a directory (grep for 'modify' and 'get')

newtype MyApp logEntry state a = MyApp {runApp ∷ ReaderT AppEnv (WriterT [logEntry] (StateT state IO)) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadWriter [logEntry], MonadState state)

runMyApp ∷ MyApp logEntry s a → AppConfig → s → IO [logEntry]
runMyApp app config = evalStateT (execWriterT (runReaderT (runApp app) (initialEnv config)))
