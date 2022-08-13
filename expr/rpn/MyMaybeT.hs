{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MyMaybeT (MaybeT (..)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..))
import Control.Monad.State (MonadIO (..), MonadState (state), MonadTrans (..))
import Control.Monad.Trans.Class ()

newtype MaybeT m a = MaybeT {runMaybeT ∷ m (Maybe a)}

instance Functor m ⇒ Functor (MaybeT m) where
  fmap ∷ (a → b) → MaybeT m a → MaybeT m b
  fmap f (MaybeT mma) = MaybeT (fmap f <$> mma)

instance Applicative m ⇒ Applicative (MaybeT m) where
  pure ∷ a → MaybeT m a
  pure a = MaybeT (pure $ Just a)

  (<*>) ∷ MaybeT m (a → b) → MaybeT m a → MaybeT m b
  (MaybeT mf) <*> (MaybeT mx) = MaybeT ((<*>) <$> mf <*> mx)

instance Monad m ⇒ Monad (MaybeT m) where
  (>>=) ∷ MaybeT m a → (a → MaybeT m b) → MaybeT m b
  (MaybeT ma) >>= f = MaybeT $ ma >>= maybe (pure Nothing) (runMaybeT . f)

instance MonadTrans MaybeT where
  lift ∷ Monad m ⇒ m a → MaybeT m a
  lift ma = MaybeT $ Just <$> ma

instance Monad m ⇒ MonadFail (MaybeT m) where
  fail ∷ String → MaybeT m a
  fail _ = MaybeT (pure Nothing)

instance MonadState s m ⇒ MonadState s (MaybeT m) where
  state ∷ (s → (a, s)) → MaybeT m a
  state = lift . state

instance Applicative m ⇒ Alternative (MaybeT m) where
  empty ∷ MaybeT m a
  empty = MaybeT (pure empty)

  (<|>) ∷ MaybeT m a → MaybeT m a → MaybeT m a
  (MaybeT mx) <|> (MaybeT my) = MaybeT ((<|>) <$> mx <*> my)

instance (Alternative m, Monad m) ⇒ MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadIO m ⇒ MonadIO (MaybeT m) where
  liftIO ∷ IO a → MaybeT m a
  liftIO = lift . liftIO
