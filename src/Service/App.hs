module Service.App
  ( ActionsService
  , Logger(..)
  , MonadMQTT(..)
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
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Client (Topic)
import Service.Env
  ( Config
  , Env
  , LogLevel(..)
  , config
  , logFilePath
  , logger
  , logLevel
  , mqttClient
  )
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


-- Logger

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
  when (level >= setLevel) $
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


-- MonadMQTT

class (Monad m, MonadReader Env m) => MonadMQTT m where
  publishMQTT :: Topic -> ByteString -> m ()

instance MonadMQTT ActionsService where
  publishMQTT topic msg =
    view mqttClient >>= \mc -> liftIO $ MQTT.publish mc topic msg False
