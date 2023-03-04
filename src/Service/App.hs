module Service.App
  ( AutomationService
  , Logger(..)
  , MonadMQTT(..)
  , log
  , logDefault
  , logWithVariant
  , runAutomationService
  , loggerConfig
  )
  where

import Prelude hiding (log)

import Control.Lens ((^.), view)
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadIO, MonadReader(..), ReaderT, liftIO, runReaderT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Network.MQTT.Client as MQTT
import Network.MQTT.Client (Topic)
import Service.Env
  ( Config
  , Env
  , LogLevel(..)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
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
import UnliftIO.STM (atomically, modifyTVar')

newtype AutomationService a = AutomationService (ReaderT Env IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadUnliftIO
    )

runAutomationService :: Env -> AutomationService a -> IO a
runAutomationService env (AutomationService x) = runReaderT x env

-- Logger

class (Monad m) => Logger m where
  debug :: Text -> m ()
  info :: Text -> m ()
  warn :: Text -> m ()
  error :: Text -> m ()

instance Logger AutomationService where
  debug = logDefault Debug
  info = logDefault Info
  warn = logDefault Warn
  error = logDefault Error

logDefault :: (MonadIO m, MonadReader Env m) => LogLevel -> Text -> m ()
logDefault level logStr = do
  setLevel <- view (config . logLevel)
  when (level >= setLevel) $ do
    logger' <- view logger
    liftIO $ logWithVariant logger' level logStr

logWithVariant :: LoggerVariant -> LogLevel -> Text -> IO ()
logWithVariant logger' level logStr =
  -- this is all for testing purposes, mostly because spinning up
  -- multiple TimedFastLogger instances at the same time seems to
  -- scramble tests ONLY when building with nix (╯°□°）╯︵ ┻━┻
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


-- MonadMQTT

class (Monad m) => MonadMQTT m where
  publishMQTT :: Topic -> ByteString -> m ()

instance MonadMQTT AutomationService where
  publishMQTT topic msg = do
    mqttClient' <- view mqttClient
    -- this is testing-motivated boilerplate
    case mqttClient' of
      MCClient mc -> liftIO $ MQTT.publish mc topic msg False
      TQClient _textTQ -> pure ()
