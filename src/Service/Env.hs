{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( module Service.Env.Config
  , AutomationEntry
  , Env(..)
  , LoggerVariant(..)
  , MQTTDispatch
  , Registrations
  , RestartConditions(..)
  , ScheduledJobs
  , Subscriptions
  , ThreadMap
  , appCleanup
  , automationBroadcast
  , config
  , daemonBroadcast
  , deviceRegistrations
  , devices
  , devicesRawJSON
  , groupRegistrations
  , groups
  , groupsRawJSON
  , initialize
  , invertRegistrations
  , loadedDevices
  , loadedGroups
  , logger
  , messageChan
  , mqttClient
  , mqttDispatch
  , notAlreadyRestarted
  , restartConditions
  , scheduledJobs
  , startupMessages
  , subscriptions
  )
where

import Control.Lens (makeFieldsNoPrefix, (^.))
import Control.Lens.Unsound (lensProduct)
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl', for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Dhall (inputFile)
import Network.MQTT.Topic (Topic, unTopic)
import qualified Service.Automation as Automation
import Service.Automation (Automation)
import Service.AutomationName (AutomationName)
import Service.Device (Device, DeviceId)
import Service.Env.Config (Config, LogLevel (..), MQTTConfig (..), automationServiceTopic,
                           configDecoder, dbPath, logFilePath, logLevel, luaScriptPath,
                           mqttConfig, statusTopic)
import Service.MQTT.Class (MQTTClient (..))
import Service.Group (Group, GroupId)
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Topic (parseTopic)
import qualified Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import System.Log.FastLogger (TimedFastLogger)
import UnliftIO.Async (Async)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, writeTChan)


data LoggerVariant
  = TFLogger TimedFastLogger
  | QLogger (TVar [Text])


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
  { _loadedDevices       :: Bool
  , _loadedGroups        :: Bool
  , _notAlreadyRestarted :: Bool
  }
  deriving (Show, Eq)

makeFieldsNoPrefix ''RestartConditions

data Env mqttClient = Env
  { _config              :: Config
  , _logger              :: LoggerVariant
  , _mqttClient          :: mqttClient
  , _mqttDispatch        :: TVar MQTTDispatch
  , _daemonBroadcast     :: TChan Daemon.Message
  , _automationBroadcast :: TChan Automation.Message
  , _messageChan         :: TChan Daemon.Message
  , _devices             :: TVar (HashMap DeviceId Device)
  , _deviceRegistrations :: TVar (Registrations DeviceId)
  , _groups              :: TVar (HashMap GroupId Group)
  , _groupRegistrations  :: TVar (Registrations GroupId)
  , _subscriptions       :: TVar Subscriptions
  , _scheduledJobs       :: TVar ScheduledJobs
  , _restartConditions   :: TVar RestartConditions
  , _startupMessages     :: TVar [Daemon.Message]
  , _devicesRawJSON      :: TVar ByteString
  , _groupsRawJSON       :: TVar ByteString
  -- do I need to mark this explicitly as being lazy so it's not called immediately?
  , _appCleanup          :: IO ()
  }

makeFieldsNoPrefix ''Env

-- TODO this needs way better error handling
initialize
  :: (MQTTClient mqttClient)
  => FilePath
  -> (Config -> IO (LoggerVariant, IO ()))
  -> (Config -> LoggerVariant -> TVar MQTTDispatch -> IO (mqttClient, IO ()))
  -> IO (Env mqttClient)
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
    <*> (newTVarIO [])      -- startupMessages
    <*> (newTVarIO "")      -- devicesRawJSON
    <*> (newTVarIO "")      -- groupsRawJSON
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
                    write daemonBroadcast' $ Daemon.DeviceUpdate devicesJSON msg
             ) :| []
            )

          , (Zigbee2MQTT.groupsTopic,
             (\msg ->
                case decode msg of
                  Just [] -> pure ()
                  Nothing -> pure ()
                  Just groupsJSON -> do
                    write daemonBroadcast' $ Daemon.GroupUpdate groupsJSON msg
             ) :| []
            )

          , (statusTopic', (const $ write daemonBroadcast' Daemon.Status) :| [])
          ]
