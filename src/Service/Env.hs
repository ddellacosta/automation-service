{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( Env(..)
  , Config(..)
  , Registrations
  , LogLevel(..)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , MQTTConfig(..)
  , MQTTDispatch
  , RestartConditions(..)
  , Subscriptions
  , appCleanup
  , automationBroadcast
  , automationServiceTopicFilter
  , caCertPath
  , clientCertPath
  , clientKeyPath
  , config
  , configDecoder
  , daemonBroadcast
  , dbPath
  , deviceRegistrations
  , devices
  , groupRegistrations
  , groups
  , initialize
  , loadedDevices
  , loadedGroups
  , logFilePath
  , logLevel
  , logger
  , luaScriptPath
  , messageChan
  , mqttClient
  , mqttConfig
  , mqttDispatch
  , notAlreadyRestarted
  , restartConditions
  , subscriptions
  , uri
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Data.Text (Text)
import Dhall (Decoder, Generic, FromDhall(..), auto, field, inputFile, record, string)
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (Filter, Topic)
import Network.URI (URI, nullURI, parseURI)
import qualified Service.Automation as Automation
import Service.AutomationName (AutomationName)
import Service.Device (Device, DeviceId)
import Service.Group (Group, GroupId)
import qualified Service.Messages.Daemon as Daemon
import qualified Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import System.Log.FastLogger (TimedFastLogger) 
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, writeTChan)

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

-- TODO maybe should at least notify the user somehow if this happens?
-- Although I guess it should probably be an initialization check
-- vs. doing that here
uriDecoder :: Decoder URI
uriDecoder = string <&> fromMaybe nullURI . parseURI

data Config = Config
  { _mqttConfig :: MQTTConfig
  , _logFilePath :: FilePath
  , _logLevel :: LogLevel
  , _luaScriptPath :: FilePath
  , _dbPath :: FilePath
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
    )

-- this is testing-motivated boilerplate
data LoggerVariant
  = TFLogger TimedFastLogger
  | QLogger (TVar [Text])

-- this is testing-motivated boilerplate
data MQTTClientVariant
  = MCClient MQTTClient
  | TVClient (TVar (HashMap Topic ByteString))

type Registrations a = HashMap a (NonEmpty AutomationName)

type MsgAction = ByteString -> IO ()
type MQTTDispatch = HashMap Topic (NonEmpty MsgAction)

type Subscriptions = HashMap AutomationName (NonEmpty (TChan Value))

data RestartConditions
  = RestartConditions
  { _loadedDevices :: Bool
  , _loadedGroups :: Bool
  , _notAlreadyRestarted :: Bool
  }
  deriving (Show, Eq)

makeFieldsNoPrefix ''RestartConditions

data Env = Env
  { _config :: Config
  , _logger :: LoggerVariant
  , _mqttClient :: MQTTClientVariant
  , _mqttDispatch :: TVar MQTTDispatch
  , _daemonBroadcast :: TChan Daemon.Message
  , _automationBroadcast :: TChan Automation.Message
  , _messageChan :: TChan Daemon.Message
  , _devices :: TVar (HashMap DeviceId Device)
  , _deviceRegistrations :: TVar (Registrations DeviceId)
  , _groups :: TVar (HashMap GroupId Group)
  , _groupRegistrations :: TVar (Registrations GroupId)
  , _subscriptions :: TVar Subscriptions
  , _restartConditions :: TVar RestartConditions
  -- do I need to mark this explicitly as being lazy so it's not called immediately?
  , _appCleanup :: IO ()
  }

makeFieldsNoPrefix ''Env

-- TODO this needs way better error handling
initialize
  :: FilePath
  -> (Config -> IO (LoggerVariant, IO ()))
  -> (Config -> LoggerVariant -> TVar MQTTDispatch -> IO (MQTTClientVariant, IO ()))
  -> IO Env
initialize configFilePath mkLogger mkMQTTClient = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config' <- inputFile configDecoder configFilePath

  daemonBroadcast' <- newBroadcastTChanIO

  (logger', loggerCleanup) <- mkLogger config'

  mqttDispatch' <- newTVarIO $ defaultTopicActions daemonBroadcast'
  (mc, mcCleanup) <- mkMQTTClient config' logger' mqttDispatch'

  automationBroadcast' <- newBroadcastTChanIO

  Env config' logger' mc mqttDispatch' daemonBroadcast' automationBroadcast'
    <$> (atomically $ dupTChan daemonBroadcast') -- messageChan
    <*> (newTVarIO M.empty) -- devices
    <*> (newTVarIO M.empty) -- deviceRegistrations
    <*> (newTVarIO M.empty) -- groups
    <*> (newTVarIO M.empty) -- groupRegistrations
    <*> (newTVarIO M.empty) -- subscriptions
    <*> (newTVarIO $ RestartConditions False False True)
    <*> pure (loggerCleanup >> mcCleanup)

  where
    write :: TChan Daemon.Message -> Daemon.Message -> IO ()
    write daemonBroadcast' = atomically . writeTChan daemonBroadcast'

    defaultTopicActions daemonBroadcast' = M.fromList
      [ ("default", (\msg -> for_ (decode msg) $ write daemonBroadcast') :| [])

      , (Zigbee2MQTT.devicesTopic, (\msg ->
            case decode msg of
              Just [] -> pure ()
              Nothing -> pure ()
              Just devicesJSON -> do
                write daemonBroadcast' $ Daemon.DeviceUpdate devicesJSON
            ) :| []
        )

      , (Zigbee2MQTT.groupsTopic, (\msg ->
            case decode msg of
              Just [] -> pure ()
              Nothing -> pure ()
              Just groupsJSON -> do
                write daemonBroadcast' $ Daemon.GroupUpdate groupsJSON
            ) :| []
        )
      ]
