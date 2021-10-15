{-# LANGUAGE ExistentialQuantification #-}

module Lang.Error.Interpreter where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM, try)
import Control.Monad.Free (Free (..))
import Lang.Error.Dsl
  ( LangF (..),
    LangScript,
  )

class Monad m => LangInterpreter m where
  onThrow :: forall a e. Exception e => e -> m a
  onTry :: forall a e. Exception e => m a -> m (Either e a)

langInterpreter :: (Monad m, LangInterpreter m) => LangScript a -> m a
langInterpreter (Pure a) = return a
langInterpreter (Free (ThrowException error next)) = onThrow error
langInterpreter (Free (RunTry action next)) = do
  value <- onTry $ langInterpreter action
  langInterpreter $ next value

instance LangInterpreter IO where
  onThrow error = throwM error
  onTry action = try action
