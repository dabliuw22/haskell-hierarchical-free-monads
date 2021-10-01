module Logger.Interpreter
  ( loggerInterpreter,
    LoggerInterpreter (..),
  )
where

import Colog.Core ((<&), LogAction (..))
import Colog.Core.IO (logStringStdout)
import Control.Monad.Free (Free (..))
import Logger.Dsl
  ( LogCtx (..),
    LogLevel (..),
    LogMsg (..),
    LoggerF (Log),
    LoggerScript,
    message,
  )

class Monad m => LoggerInterpreter m where
  onLog :: LogLevel -> LogCtx -> LogMsg -> m ()

loggerInterpreter :: (Monad m, LoggerInterpreter m) => LoggerScript a -> m a
loggerInterpreter (Pure a) = return a
loggerInterpreter (Free (Log level ctx msg value)) = do
  onLog level ctx msg
  loggerInterpreter value

instance LoggerInterpreter IO where
  onLog level ctx msg = logStringStdout <& message level ctx msg
