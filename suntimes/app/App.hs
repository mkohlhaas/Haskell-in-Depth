{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT, NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Types (WebAPIAuth)

-- turn off logging
-- newtype MyApp a = MyApp {runApp ∷ ReaderT WebAPIAuth (NoLoggingT IO) a}
-- runMyApp app config = runNoLoggingT $ runReaderT (runApp app) config

newtype MyApp a = MyApp {runApp ∷ ReaderT WebAPIAuth (LoggingT IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadMask, MonadReader WebAPIAuth)

runMyApp ∷ MyApp a → WebAPIAuth → IO a
runMyApp app config = runStderrLoggingT $ runReaderT (runApp app) config
