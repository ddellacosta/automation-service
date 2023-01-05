{-# LANGUAGE ImpredicativeTypes #-}

module Service.App
  ( ActionsService
  , Logger(..)
  , log
  , runActionsService
  , loggerConfig
  )
  where

import Prelude hiding (log)

import Control.Lens ((^.), view)
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader(..), ReaderT, liftIO, runReaderT)
import Data.Text (Text)
import Service.Env (Config, Env, LogLevel(..), config, logFilePath, logger, logLevel)
import System.Log.FastLogger
  ( FileLogSpec(..)
  , FormattedTime
  , LogType
  , LogType'(..)
  , TimedFastLogger
  , ToLogStr(..)
  , defaultBufSize
  , newTimeCache
  , simpleTimeFormat
  )

newtype ActionsService a = ActionsService (ReaderT Env IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadUnliftIO
    )

runActionsService :: Env -> ActionsService a -> IO a
runActionsService env (ActionsService x) = runReaderT x env

class (MonadIO m, MonadReader Env m) => Logger m where
  debug :: Text -> m ()
  info :: Text -> m ()
  warn :: Text -> m ()
  error :: Text -> m ()

instance Logger ActionsService where
  debug = logDefault Debug
  info = logDefault Info
  warn = logDefault Warn
  error = logDefault Error

logDefault :: (MonadIO m, MonadReader Env m) => LogLevel -> Text -> m ()
logDefault level logStr = do
  setLevel <- view (config . logLevel)
  when (setLevel < level) $
    view logger >>= \logger' -> liftIO $ log logger' level logStr

log :: (ToLogStr s) => TimedFastLogger -> LogLevel -> s -> IO ()
log logger' level logStr = logger' $ \time ->
  toLogStr (show level) <> " - " <> toLogStr time <> " - " <> toLogStr logStr <> "\n"

{-|
  Given a Env.Config, returns an IO-wrapped (IO FormattedTime,
  LogType), used in the creation of a TimedFastLogger. TODO:
  FileLogSpec details
-} 
loggerConfig :: Config -> IO (IO FormattedTime, LogType)
loggerConfig config' = do
  fmtTime <- newTimeCache simpleTimeFormat

  let logFilePath' = config' ^. logFilePath 
      -- TODO make these config options or constants or something
      logType = LogFile (FileLogSpec logFilePath' 1048576 50) defaultBufSize

  pure (fmtTime, logType)

--

-- class MonadMQTT m where
--   publishMQTT :: Topic -> 
