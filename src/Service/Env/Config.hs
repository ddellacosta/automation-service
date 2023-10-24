{-# LANGUAGE TemplateHaskell #-}

module Service.Env.Config
  ( Config(..)
  , LogLevel(..)
  , MQTTConfig(..)
  , automationServiceTopic
  , caCertPath
  , clientCertPath
  , clientKeyPath
  , configDecoder
  , dbPath
  , httpPort
  , logFilePath
  , logLevel
  , luaScriptPath
  , mqttConfig
  , mqttConfigDecoder
  , statusTopic
  , uri
  , uriDecoder
  )
where

import Control.Lens (makeFieldsNoPrefix, (<&>))
import Data.Maybe (fromMaybe)
import Dhall (Decoder, FromDhall (..), Generic, auto, field, record, strictText, string)
import Network.MQTT.Topic (Topic)
import Network.URI (URI, nullURI, parseURI)
import Service.AutomationName (Port (..))
import Service.MQTT.Topic (parseTopic)

data LogLevel = Debug | Info | Warn | Error
  deriving (Generic, Show, Eq, Ord)

instance FromDhall LogLevel

data MQTTConfig = MQTTConfig
  { _uri                    :: URI
  , _automationServiceTopic :: Topic
  , _statusTopic            :: Topic
  , _caCertPath             :: Maybe FilePath
  , _clientCertPath         :: Maybe FilePath
  , _clientKeyPath          :: Maybe FilePath
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''MQTTConfig

mqttConfigDecoder :: Decoder MQTTConfig
mqttConfigDecoder =
  record
    ( MQTTConfig
        <$> field "uri" uriDecoder
        <*> field "automationServiceTopic" (strictText <&> parseTopic)
        <*> field "statusTopic" (strictText <&> parseTopic)
        <*> field "caCertPath" auto
        <*> field "clientCertPath" auto
        <*> field "clientKeyPath" auto
    )

-- TODO maybe should at least notify the user somehow if this happens?
-- Although I guess it should probably be an initialization check
-- vs. doing that here
uriDecoder :: Decoder URI
uriDecoder = string <&> fromMaybe nullURI . parseURI

data Config = Config
  { _mqttConfig    :: MQTTConfig
  , _logFilePath   :: FilePath
  , _logLevel      :: LogLevel
  , _luaScriptPath :: FilePath
  , _dbPath        :: FilePath
  , _httpPort      :: Port
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
        <*> field "dbPath" string
        <*> field "httpPort" (Port <$> auto)
    )
