{-# LANGUAGE GADTs #-}

module Resource.Dsl where

import Control.Monad.Free (Free (..))
import Logger.Dsl (LoggerScript)

newtype Directory = Directory String deriving (Show)

mkDirectory :: String -> Directory
mkDirectory = Directory

newtype Line = Line {line :: String} deriving (Show)

mkLine :: String -> Line
mkLine = Line

instance Semigroup Line where
  (Line x) <> (Line "") = Line x
  (Line "") <> (Line y) = Line y
  (Line x) <> (Line y) = Line (x <> "\n" <> y)

data Extension
  = All
  | Custom String
  deriving (Show, Eq)

mkAll :: Extension
mkAll = All

mkCustom :: String -> Extension
mkCustom = Custom

newtype Filename = Filename String deriving (Show)

mkFilename :: String -> Filename
mkFilename = Filename

data File = File Directory Filename [Line] deriving (Show)

mkFile :: Directory -> Filename -> [Line] -> File
mkFile = File

data ResourceF a where
  Get :: LoggerScript () -> Directory -> Extension -> ([File] -> a) -> ResourceF a
  Put :: LoggerScript () -> File -> a -> ResourceF a
  Create :: LoggerScript () -> Directory -> (Directory -> a) -> ResourceF a

instance Functor ResourceF where
  fmap f (Get logger directory ext g) = Get logger directory ext (f . g)
  fmap f (Put logger file a) = Put logger file (f a)
  fmap f (Create logger directory g) = Create logger directory (f . g)

type ResourceScript a = Free ResourceF a

get :: LoggerScript () -> Directory -> Extension -> ResourceScript [File]
get logger directory extension = Free $ Get logger directory extension pure

put :: LoggerScript () -> File -> Directory -> ResourceScript ()
put logger file directory = Free $ Put logger file (pure ())

create :: LoggerScript () -> Directory -> ResourceScript Directory
create logger directory = Free $ Create logger directory pure
