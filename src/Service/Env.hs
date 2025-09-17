{-# LANGUAGE TemplateHaskell #-}

module Service.Env
  ( module Service.Env.Config
  , AutomationEntry
  , Env(..)
  , Subscriptions
  , Registrations
  , RestartConditions(..)
  , ScheduledJobs
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
  , subscriptions
  , notAlreadyRestarted
  , restartConditions
  , scheduledJobs
  , startupMessages
  )
where

import Control.Lens (makeFieldsNoPrefix, (^.))
import Control.Lens.Unsound (lensProduct)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldl', for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)))
import Dhall (inputFile)
import Network.MQTT.Topic (Topic, unTopic)
import Service.App.Logger (Logger (..))
import qualified Service.Automation as Automation
import Service.Automation (Automation)
import Service.AutomationName (AutomationName(..))
import Service.Device (Device, DeviceId)
import Service.Env.Config (Config, LogLevel (..), MQTTConfig (..), automationServiceTopic,
                           configDecoder, dbPath, httpPort, logFilePath, logLevel, luaScriptPath,
                           mqttConfig, statusTopic)
import Service.Group (Group, GroupId)
import Service.MQTT.Class (MQTTClient (..))
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Topic (parseTopic)
import qualified Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import UnliftIO.Async (Async)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, writeTChan)

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


type MsgAction = Topic -> ByteString -> IO ()
type Subscriptions = HashMap Topic (HashMap AutomationName MsgAction)

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

data Env logger mqttClient = Env
  { _config              :: Config
  , _logger              :: logger
  , _mqttClient          :: mqttClient
  , _subscriptions       :: TVar Subscriptions
  , _daemonBroadcast     :: TChan Daemon.Message
  , _automationBroadcast :: TChan Automation.Message
  , _messageChan         :: TChan Daemon.Message
  , _devices             :: TVar (HashMap DeviceId Device)
  , _deviceRegistrations :: TVar (Registrations DeviceId)
  , _groups              :: TVar (HashMap GroupId Group)
  , _groupRegistrations  :: TVar (Registrations GroupId)
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
  :: (Logger logger, MQTTClient mqttClient)
  => FilePath
  -> (Config -> IO (logger, IO ()))
  -> (Config -> logger -> TVar Subscriptions -> IO (mqttClient, IO ()))
  -> IO (Env logger mqttClient)
initialize configFilePath mkLogger mkMQTTClient = do
  -- need to handle a configuration error? Dhall provides a lot of error output
  config' <- inputFile configDecoder configFilePath

  daemonBroadcast' <- newBroadcastTChanIO

  (logger', loggerCleanup) <- mkLogger config'

  subscriptions' <- newTVarIO $ defaultTopicActions config' daemonBroadcast'
  (mc, mcCleanup) <- mkMQTTClient config' logger' subscriptions'

  automationBroadcast' <- newBroadcastTChanIO

  Env config' logger' mc subscriptions' daemonBroadcast' automationBroadcast'
    <$> (atomically $ dupTChan daemonBroadcast') -- messageChan
    <*> (newTVarIO M.empty) -- devices
    <*> (newTVarIO M.empty) -- deviceRegistrations
    <*> (newTVarIO M.empty) -- groups
    <*> (newTVarIO M.empty) -- groupRegistrations
    <*> (newTVarIO M.empty) -- scheduledJobs
    <*> (newTVarIO $ RestartConditions False False True)
    <*> (newTVarIO [])      -- startupMessages
    <*> (newTVarIO "")      -- devicesRawJSON
    <*> (newTVarIO "")      -- groupsRawJSON
    <*> pure (loggerCleanup >> mcCleanup)


defaultTopicActions :: Config -> TChan Daemon.Message -> Subscriptions
defaultTopicActions config' daemonBroadcast' =
  let
    (automationServiceTopic', statusTopic') =
      config' ^. mqttConfig . lensProduct automationServiceTopic statusTopic
    setTopic = parseTopic . (<> "/set") . unTopic $ automationServiceTopic'
  in
    M.fromList
      [ ( setTopic
        , M.singleton Null (\_topic msg -> for_ (decode msg) $ write daemonBroadcast')
        )

      , ( Zigbee2MQTT.devicesTopic
        , M.singleton Null
          (\_topic msg ->
             case decode msg of
               Just [] -> pure ()
               Nothing -> pure ()
               Just devicesJSON ->
                 write daemonBroadcast' $ Daemon.DeviceUpdate devicesJSON msg
          )
        )

      , ( Zigbee2MQTT.groupsTopic
        , M.singleton Null
          (\_topic msg ->
             case decode msg of
               Just [] -> pure ()
               Nothing -> pure ()
               Just groupsJSON ->
                 write daemonBroadcast' $ Daemon.GroupUpdate groupsJSON msg
          )
        )

      , ( statusTopic'
        , M.singleton Null (\_topic _msg -> write daemonBroadcast' Daemon.Status)
        )
      ]
  where
    write :: TChan Daemon.Message -> Daemon.Message -> IO ()
    write daemonBroadcast'' = atomically . writeTChan daemonBroadcast''
