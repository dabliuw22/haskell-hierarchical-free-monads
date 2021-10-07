{-# LANGUAGE TupleSections #-}

module Resource.Interpreter where

import Control.Monad.Free (Free (..))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toUpper)
import Logger.Interpreter
import Resource.Dsl
  ( Directory (..),
    Extension (..),
    File (..),
    Filename (..),
    Line (..),
    ResourceF (..),
    ResourceScript,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath
  ( (</>),
    takeBaseName,
    takeExtension,
  )
import System.IO
  ( FilePath,
    readFile,
    writeFile,
  )

class Monad m => ResourceInterpreter m where
  onGet :: Directory -> Extension -> m [File]
  onPut :: File -> m ()
  onCreate :: Directory -> m Directory

resourceInterpreter :: (Monad m, ResourceInterpreter m, LoggerInterpreter m) => ResourceScript a -> m a
resourceInterpreter (Pure a) = pure a
resourceInterpreter (Free (Get logger directory extension next)) = do
  _ <- loggerInterpreter logger
  files <- onGet directory extension
  resourceInterpreter (next files)
resourceInterpreter (Free (Put logger file value)) = do
  _ <- loggerInterpreter logger
  _ <- onPut file
  resourceInterpreter value
resourceInterpreter (Free (Create logger directory next)) = do
  _ <- loggerInterpreter logger
  value <- onCreate directory
  resourceInterpreter (next value)

instance ResourceInterpreter IO where
  onGet dir@(Directory directory) extension = do
    values <- listDirectory directory
    let filepaths = (directory </>) <$> clean values extension
    files <- read filepaths
    return $ tupleToFile dir <$> files
    where
      clean :: [FilePath] -> Extension -> [FilePath]
      clean filepaths' All = filepaths'
      clean filepaths' (Custom ext) = filter (\a -> takeExtension a == ext) filepaths'
      read :: [FilePath] -> IO [(FilePath, String)]
      read [] = pure []
      read filepaths' = sequence $ (\filepath' -> (filepath',) <$> readFile filepath') <$> filepaths'
      tupleToFile :: Directory -> (FilePath, String) -> File
      tupleToFile directory' (fp, s) =
        File directory' (Filename $ takeBaseName fp) (Line <$> lines s)
  onPut (File (Directory directory) (Filename name) lines) = do
    let filename = directory </> name
    writeFile filename (readLines lines)
    where
      readLines :: [Line] -> String
      readLines [] = ""
      readLines lines' = line $ getLine lines'
      getLine :: [Line] -> Line
      getLine lines' = foldl (<>) (Line "") lines'
  onCreate dir@(Directory directory) = do
    _ <- remove directory
    _ <- createDirectoryIfMissing True directory
    pure dir
    where
      remove :: FilePath -> IO ()
      remove directory' = do
        exists <- doesDirectoryExist directory'
        if exists
          then removeDirectoryRecursive directory'
          else pure ()
