{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( Env
  , Env'(..)
  , Config(..)
  , LogLevel(..)
  , MQTTConfig(..)
  , automationServiceTopicFilter
  , caCertPath
  , clientCertPath
  , clientKeyPath
  , config
  , configDecoder
  , devices
  , logFilePath
  , logLevel
  , logger
  , luaScriptPath
  , appCleanup
  , messageQueue
  , mqttClient
  , mqttConfig
  , uri
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (Value)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Dhall (Decoder, Generic, FromDhall(..), auto, field, record, string)
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (Filter)
import Network.URI (URI, nullURI, parseURI)
import Service.Device (Device)
import qualified Service.Messages.Daemon as Daemon
import qualified Service.Messages.Zigbee2MQTTDevice as Zigbee2MQTTDevice
import System.Log.FastLogger (TimedFastLogger) 
import UnliftIO.STM (TQueue, TVar)

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

data Env' logger mqttClient = Env'
  { _config :: Config
  , _logger :: logger
  , _mqttClient :: mqttClient
  , _messageQueue :: TQueue Daemon.Message
  , _appCleanup :: IO ()
  , _devices :: TVar [Device]
  , _deviceMessageQueue :: TQueue Zigbee2MQTTDevice.Message
  }

makeFieldsNoPrefix ''Env'

type Env = Env' TimedFastLogger MQTTClient
