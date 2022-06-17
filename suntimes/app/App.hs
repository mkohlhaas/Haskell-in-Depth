{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..))
import Types (WebAPIAuth)

newtype MyApp a = MyApp {runApp :: ReaderT WebAPIAuth IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadReader WebAPIAuth)

runMyApp :: MyApp a -> WebAPIAuth -> IO a
runMyApp app config = runReaderT (runApp app) config
