{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( AutomationEntry
  , Config(..)
  , Env(..)
  , LogLevel(..)
  , LoggerVariant(..)
  , MQTTClientVariant(..)
  , MQTTConfig(..)
  , MQTTDispatch
  , Registrations
  , RestartConditions(..)
  , ScheduledJobs
  , Subscriptions
  , ThreadMap
  , appCleanup
  , automationBroadcast
  , automationServiceTopic
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
  , invertRegistrations
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
  , scheduledJobs
  , startupMessages
  , statusTopic
  , subscriptions
  , uri
  )
where

import Control.Lens ((<&>), (^.), makeFieldsNoPrefix)
import Control.Lens.Unsound (lensProduct)
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl', for_)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Dhall (Decoder, Generic, FromDhall(..), auto, field, inputFile, record, strictText, string)
import Network.MQTT.Client (MQTTClient)
import Network.MQTT.Topic (Topic, unTopic)
import Network.URI (URI, nullURI, parseURI)
import qualified Service.Automation as Automation
import Service.Automation (Automation)
import Service.AutomationName (AutomationName)
import Service.Device (Device, DeviceId)
import Service.Group (Group, GroupId)
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Topic (parseTopic)
import qualified Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import System.Log.FastLogger (TimedFastLogger) 
import UnliftIO.Async (Async)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, writeTChan)


data LogLevel = Debug | Info | Warn | Error
  deriving (Generic, Show, Eq, Ord)

instance FromDhall LogLevel

data MQTTConfig = MQTTConfig
  { _uri :: URI
  , _automationServiceTopic :: Topic
  , _statusTopic :: Topic
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

-- in here to avoid a circular reference between Service.Daemon and
-- Service.MQTT.Status, otherwise I'd leave it in Service.Daemon

type AutomationEntry m = (Automation m, Async ())
type ThreadMap m = HashMap AutomationName (AutomationEntry m)

--

type Registrations a = HashMap a (NonEmpty AutomationName)

invertRegistrations :: Registrations a -> HashMap AutomationName (NonEmpty a)
invertRegistrations = M.foldlWithKey'
  (\inverted k ->
     foldl' (\inverted' v -> M.insertWith (<>) v (k :| []) inverted') inverted)
  M.empty


type MsgAction = ByteString -> IO ()
type MQTTDispatch = HashMap Topic (NonEmpty MsgAction)

type Subscriptions = HashMap AutomationName (NonEmpty (TChan Value))

type ScheduledJobs =
  HashMap Daemon.JobId (Daemon.AutomationSchedule, Daemon.Message, ThreadId)

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
  , _scheduledJobs :: TVar ScheduledJobs
  , _restartConditions :: TVar RestartConditions
  , _startupMessages :: TVar [Daemon.Message]
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

  mqttDispatch' <- newTVarIO $ defaultTopicActions config' daemonBroadcast'
  (mc, mcCleanup) <- mkMQTTClient config' logger' mqttDispatch'

  automationBroadcast' <- newBroadcastTChanIO

  Env config' logger' mc mqttDispatch' daemonBroadcast' automationBroadcast'
    <$> (atomically $ dupTChan daemonBroadcast') -- messageChan
    <*> (newTVarIO M.empty) -- devices
    <*> (newTVarIO M.empty) -- deviceRegistrations
    <*> (newTVarIO M.empty) -- groups
    <*> (newTVarIO M.empty) -- groupRegistrations
    <*> (newTVarIO M.empty) -- subscriptions
    <*> (newTVarIO M.empty) -- scheduledJobs
    <*> (newTVarIO $ RestartConditions False False True)
    <*> (newTVarIO []) -- startupMessages
    <*> pure (loggerCleanup >> mcCleanup)

  where
    write :: TChan Daemon.Message -> Daemon.Message -> IO ()
    write daemonBroadcast' = atomically . writeTChan daemonBroadcast'

    defaultTopicActions config' daemonBroadcast' =
      let
        (automationServiceTopic', statusTopic') =
          config' ^. mqttConfig . lensProduct automationServiceTopic statusTopic
        setTopic = parseTopic . (<> "/set") . unTopic $ automationServiceTopic'
      in
        M.fromList
          [ (setTopic, (\msg -> for_ (decode msg) $ write daemonBroadcast') :| [])

          , (Zigbee2MQTT.devicesTopic,
             (\msg ->
                case decode msg of
                  Just [] -> pure ()
                  Nothing -> pure ()
                  Just devicesJSON -> do
                    write daemonBroadcast' $ Daemon.DeviceUpdate devicesJSON
                ) :| []
            )

          , (Zigbee2MQTT.groupsTopic,
             (\msg ->
                case decode msg of
                  Just [] -> pure ()
                  Nothing -> pure ()
                  Just groupsJSON -> do
                    write daemonBroadcast' $ Daemon.GroupUpdate groupsJSON
                ) :| []
            )

          , (statusTopic', (const $ write daemonBroadcast' Daemon.Status) :| [])
          ]
