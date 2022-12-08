{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT, NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Types (WebAPIAuth)

-- to turn off logging
-- newtype MyApp a = MyApp {runApp ∷ ReaderT WebAPIAuth (NoLoggingT IO) a}
-- runMyApp = (runNoLoggingT .) . runReaderT . runApp

newtype MyApp a = MyApp {runApp ∷ ReaderT WebAPIAuth (LoggingT IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadMask, MonadReader WebAPIAuth)

runMyApp ∷ MyApp a → WebAPIAuth → IO a
runMyApp = (runStderrLoggingT .) . runReaderT . runApp
