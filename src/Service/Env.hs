{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( Env
  , Env'(..)
  , Config(..)
  , LogLevel(..)
  , MQTTConfig(..)
  , actionsServiceTopicFilter
  , caCertPath
  , clientCertPath
  , clientKeyPath
  , config
  , configDecoder
  , devices
  , logFilePath
  , logLevel
  , logger
  , appCleanup
  , messageQueue
  , mqttClient
  , mqttConfig
  , uri
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Dhall (Decoder, Generic, FromDhall(..), auto, list, strictText, field, record, string)
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (Filter)
import Network.URI (URI, nullURI, parseURI)
import Service.Device (Device(..), parseDeviceId)
import qualified Service.Messages.Action as Messages
import System.Log.FastLogger (TimedFastLogger) 
import UnliftIO.STM (TQueue)

data LogLevel = Debug | Info | Warn | Error
  deriving (Generic, Show, Eq, Ord)

instance FromDhall LogLevel

deviceDecoder :: Decoder Device
deviceDecoder =
  record
    ( Device
        <$> field "id" (strictText <&> parseDeviceId)
        <*> field "name" strictText
        <*> field "topic" (string <&> S.fromString)
    )

data MQTTConfig = MQTTConfig
  { _uri :: URI
  , _actionsServiceTopicFilter :: Filter
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
        <*> field "actionsServiceTopic" (string <&> S.fromString)
        <*> field "caCertPath" auto
        <*> field "clientCertPath" auto
        <*> field "clientKeyPath" auto
    )

uriDecoder :: Decoder URI
uriDecoder = string <&> (fromMaybe nullURI) . parseURI

data Config = Config
  { _mqttConfig :: MQTTConfig
  , _devices :: [Device]
  , _logFilePath :: FilePath
  , _logLevel :: LogLevel
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''Config

configDecoder :: Decoder Config
configDecoder =
  record
    ( Config
        <$> field "mqttBroker" mqttConfigDecoder
        <*> field "devices" (list deviceDecoder)
        <*> field "logFilePath" string 
        <*> field "logLevel" auto
    )

data Env' logger mqttClient = Env'
  { _config :: Config
  , _logger :: logger
  , _mqttClient :: mqttClient
  , _messageQueue :: TQueue Messages.Action
  , _appCleanup :: IO ()
  }

makeFieldsNoPrefix ''Env'

type Env = Env' TimedFastLogger MQTTClient
