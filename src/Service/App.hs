module Service.App
  ( module Service.App.Logger
  , AutomationService
  , debug
  , error
  , findDeviceM
  , info
  , logDefault
  , loggerConfig
  , publish
  , runAutomationService
  , subscribe
  , unsubscribe
  , warn
  )
  where

import Prelude hiding (error, log)

import Control.Lens (view, (^.))
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader (..), ReaderT, liftIO, runReaderT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Network.MQTT.Client (Topic)
import Service.App.Logger (Logger (..))
import Service.Device (Device, DeviceId)
import Service.Env (Config, Env, LogLevel (..), config, devices, logFilePath, logLevel, logger,
                    mqttClient)
import Service.MQTT.Class (MQTTClient (..))
import System.Log.FastLogger (FileLogSpec (..), FormattedTime, LogType, LogType' (..),
                              defaultBufSize, newTimeCache, simpleTimeFormat)
import UnliftIO.STM (atomically, readTVar)

newtype AutomationService logger mqttClient a
  = AutomationService (ReaderT (Env logger mqttClient) IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env logger mqttClient)
    , MonadUnliftIO
    )

runAutomationService :: Env logger mqttClient -> AutomationService logger mqttClient a -> IO a
runAutomationService env (AutomationService x) = runReaderT x env

-- Logger

debug, info, warn, error
  :: (Logger logger, MonadIO m, MonadReader (Env logger mqttClient) m)
  => Text
  -> m ()
debug = logDefault Debug
info = logDefault Info
warn = logDefault Warn
error = logDefault Error

logDefault
  :: (Logger logger, MonadIO m, MonadReader (Env logger mqttClient) m)
  => LogLevel
  -> Text
  -> m ()
logDefault level logStr = do
  setLevel <- view (config . logLevel)
  when (level >= setLevel) $ do
    logger' <- view logger
    liftIO $ log logger' level logStr

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
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadIO m)
  => Topic
  -> ByteString
  -> m ()
publish topic msg =
  view mqttClient >>= \mc -> liftIO $ publishMQTT mc topic msg

subscribe
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadIO m)
  => Topic
  -> m ()
subscribe topic =
  view mqttClient >>= \mc -> liftIO $ subscribeMQTT mc topic

unsubscribe
  :: (Logger l, MQTTClient mc, MonadReader (Env l mc) m, MonadIO m)
  => Topic
  -> m ()
unsubscribe topic =
  view mqttClient >>= \mc -> liftIO $ unsubscribeMQTT mc topic


-- not sure where to put this, but eventually I want to just get rid
-- of it
findDeviceM
  :: (MonadIO m, MonadReader (Env logger mqttClient) m)
  => DeviceId
  -> m (Maybe Device)
findDeviceM deviceId = do
  storedDevices <- view devices
  storedDevices' <- atomically $ readTVar storedDevices
  pure $ M.lookup deviceId storedDevices'
