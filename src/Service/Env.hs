{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( Env(..)
  , Config(..)
  , LogLevel(..)
  , MQTTConfig(..)
  , actionsServiceTopicFilter
  , caCertPath -- currently unused
  , clientCertPath -- currently unused
  , clientKeyPath -- currently unused
  , config
  , configDecoder
  , devices
  , logFilePath
  , logLevel -- currently unused
  , logger
  , loggerCleanup
  , messagesChan
  , mqttClient
  , mqttConfig
  , uri -- currently unused
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Data.Text (Text)
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
  , _caCertPath :: FilePath
  , _clientCertPath :: FilePath
  , _clientKeyPath :: FilePath
  , _actionsServiceTopicFilter :: Filter
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''MQTTConfig

mqttConfigDecoder :: Decoder MQTTConfig
mqttConfigDecoder =
  record
    ( MQTTConfig
        <$> field "uri" uriDecoder
        <*> field "caCertPath" string
        <*> field "clientCertPath" string
        <*> field "clientKeyPath" string
        <*> field "actionsServiceTopic" (string <&> S.fromString)
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

data Env = Env
  { _config :: Config
  , _logger :: TimedFastLogger
  , _loggerCleanup :: IO ()
  , _mqttClient :: MQTTClient
  , _messagesChan :: TQueue (Messages.Action Text)
  }
--  deriving (Generic)

makeFieldsNoPrefix ''Env
