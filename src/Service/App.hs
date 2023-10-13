module Service.App
  ( AutomationService
  , Logger(..)
  , findDeviceM
  , log
  , logDefault
  , logWithVariant
  , loggerConfig
  , publish
  , runAutomationService
  , subscribe
  )
  where

import Prelude hiding (log)

import Control.Lens (view, (^.))
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader (..), ReaderT, liftIO, runReaderT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.MQTT.Client (Topic)
import Service.Device (Device, DeviceId)
import Service.Env (Config, Env, LogLevel (..), LoggerVariant (..), config,
                    devices, logFilePath, logLevel, logger, mqttClient)
import Service.MQTT.Class (MQTTClient (..))
import System.Log.FastLogger (FileLogSpec (..), FormattedTime, LogType, LogType' (..),
                              TimedFastLogger, ToLogStr (..), defaultBufSize, newTimeCache,
                              simpleTimeFormat)
import UnliftIO.STM (atomically, modifyTVar', readTVar)

newtype AutomationService mqttClient a = AutomationService (ReaderT (Env mqttClient) IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env mqttClient)
    , MonadUnliftIO
    )

runAutomationService :: Env mqttClient -> AutomationService mqttClient a -> IO a
runAutomationService env (AutomationService x) = runReaderT x env

-- Logger

class (Monad m) => Logger m where
  debug :: Text -> m ()
  info :: Text -> m ()
  warn :: Text -> m ()
  error :: Text -> m ()

instance Logger (AutomationService mqttClient) where
  debug = logDefault Debug
  info = logDefault Info
  warn = logDefault Warn
  error = logDefault Error

logDefault :: (MonadIO m, MonadReader (Env mqttClient) m) => LogLevel -> Text -> m ()
logDefault level logStr = do
  setLevel <- view (config . logLevel)
  when (level >= setLevel) $ do
    logger' <- view logger
    liftIO $ logWithVariant logger' level logStr

logWithVariant :: LoggerVariant -> LogLevel -> Text -> IO ()
logWithVariant logger' level logStr =
  -- LoggerVariant and related boilerplate is all for testing
  -- purposes, mostly because spinning up multiple TimedFastLogger
  -- instances at the same time seems to scramble tests ONLY when
  -- building with nix (╯°□°）╯︵ ┻━┻
  case logger' of
    TFLogger tfLogger -> log tfLogger level $ logStr
    QLogger qLogger -> atomically . modifyTVar' qLogger $ \msgs ->
      msgs <> [ T.pack (show level) <> ": " <> logStr ]

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


-- MQTTClient helpers

publish
  :: (MQTTClient mc, MonadReader (Env mc) m, MonadIO m) => Topic -> ByteString -> m ()
publish topic msg =
  view mqttClient >>= \mc -> liftIO $ publishMQTT mc topic msg

subscribe
  :: (MQTTClient mc, MonadReader (Env mc) m, MonadIO m) => Topic -> m ()
subscribe topic =
  view mqttClient >>= \mc -> liftIO $ subscribeMQTT mc topic


-- not sure where to put this, but eventually I want to just get rid
-- of it
findDeviceM
  :: (MonadIO m, MonadReader (Env mqttClient) m)
  => DeviceId
  -> m (Maybe Device)
findDeviceM deviceId = do
  storedDevices <- view devices
  storedDevices' <- atomically $ readTVar storedDevices
  pure $ M.lookup deviceId storedDevices'
