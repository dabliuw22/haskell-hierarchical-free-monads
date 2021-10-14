{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Lang.Error.Dsl where

import Control.Exception (Exception)
import Control.Monad.Free (Free (..), liftF)

data LangF next where
  ThrowException :: forall a e next. Exception e => e -> (a -> next) -> LangF next
  RunTry :: forall a e next. Exception e => LangScript a -> (Either e a -> next) -> LangF next

instance Functor LangF where
  fmap f (ThrowException error next) = ThrowException error (f . next)
  fmap f (RunTry action next) = RunTry action (f . next)

type LangScript a = Free LangF a

throwException :: forall a e. Exception e => e -> LangScript a
throwException error = liftF $ ThrowException error id

runTry :: forall a e. Exception e => LangScript a -> LangScript (Either e a)
runTry action = liftF $ RunTry action id
