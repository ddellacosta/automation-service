{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( Env(..)
  , Config(..)
  , DeviceRegistrations
  , LogLevel(..)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , MQTTConfig(..)
  , appCleanup
  , automationBroadcast
  , automationServiceTopicFilter
  , caCertPath
  , clientCertPath
  , clientKeyPath
  , config
  , configDecoder
  , daemonBroadcast
  , deviceRegistrations
  , devices
  , initialize
  , logFilePath
  , logLevel
  , logger
  , luaScriptPath
  , messageChan
  , mqttClient
  , mqttConfig
  , serverChan
  , uri
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Data.Text (Text)
import Dhall (Decoder, Generic, FromDhall(..), auto, field, inputFile, record, string)
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (Filter)
import Network.URI (URI, nullURI, parseURI)
import qualified Service.Automation as Automation
import Service.AutomationName (AutomationName)
import Service.Device (Device, DeviceId)
import qualified Service.Messages.Daemon as Daemon
import System.Log.FastLogger (TimedFastLogger) 
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO)

data LogLevel = Debug | Info | Warn | Error
  deriving (Generic, Show, Eq, Ord)

instance FromDhall LogLevel

data MQTTConfig = MQTTConfig
  { _uri :: URI
  , _automationServiceTopicFilter :: Filter
  , _caCertPath :: Maybe FilePath
  , _clientCertPath :: Maybe FilePath
  , _clientKeyPath :: Maybe FilePath
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''MQTTConfig

mqttConfigDecoder :: Decoder MQTTConfig
mqttConfigDecoder =
  record
    ( MQTTConfig
        <$> field "uri" uriDecoder
        <*> field "automationServiceTopic" (string <&> S.fromString)
        <*> field "caCertPath" auto
        <*> field "clientCertPath" auto
        <*> field "clientKeyPath" auto
    )

uriDecoder :: Decoder URI
uriDecoder = string <&> fromMaybe nullURI . parseURI

data Config = Config
  { _mqttConfig :: MQTTConfig
  , _logFilePath :: FilePath
  , _logLevel :: LogLevel
  , _luaScriptPath :: FilePath
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''Config

configDecoder :: Decoder Config
configDecoder =
  record
    ( Config
        <$> field "mqttBroker" mqttConfigDecoder
        <*> field "logFilePath" string 
        <*> field "logLevel" auto
        <*> field "luaScriptPath" string
    )

-- this is testing-motivated boilerplate
data LoggerVariant
  = TFLogger TimedFastLogger
  | QLogger (TVar [Text])

-- this is testing-motivated boilerplate
data MQTTClientVariant
  = MCClient MQTTClient
  | TQClient (TVar (Map Text [Text]))

type DeviceRegistrations = Map DeviceId AutomationName

data Env = Env
  { _config :: Config
  , _logger :: LoggerVariant
  , _mqttClient :: MQTTClientVariant
  , _daemonBroadcast :: TChan Daemon.Message
  , _messageChan :: TChan Daemon.Message
  , _devices :: TVar (Map DeviceId Device)
  , _deviceRegistrations :: TVar DeviceRegistrations
  , _automationBroadcast :: TChan Automation.Message
  , _serverChan :: TChan Automation.Message
  , _appCleanup :: IO ()
  }

makeFieldsNoPrefix ''Env

-- TODO this needs way better error handling
initialize
  :: FilePath
  -> (Config -> IO (LoggerVariant, IO ()))
  -> (Config -> LoggerVariant -> TChan Daemon.Message -> IO (MQTTClientVariant, IO ()))
  -> IO Env
initialize configFilePath mkLogger mkMQTTClient = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config' <- inputFile configDecoder configFilePath

  daemonBroadcast' <- newBroadcastTChanIO
  messageChan' <- atomically $ dupTChan daemonBroadcast'

  (logger', loggerCleanup) <- mkLogger config'

  (mc, mcCleanup) <- mkMQTTClient config' logger' messageChan'

  devices' <- newTVarIO M.empty
  deviceRegistrations' <- newTVarIO M.empty

  automationBroadcast' <- newBroadcastTChanIO
  serverChan' <- atomically $ dupTChan automationBroadcast'

  pure $
    Env
      config'
      logger'
      mc
      daemonBroadcast'
      messageChan'
      devices'
      deviceRegistrations'
      automationBroadcast'
      serverChan'
      (loggerCleanup >> mcCleanup)
