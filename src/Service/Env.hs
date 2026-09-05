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

-- import Service.STM.Stats

import Control.Monad.IO.Unlift (liftIO)

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
                           configDecoder, dbPath, httpPort, httpRoot, logFilePath, logLevel,
                           luaScriptPath, mqttConfig, statusTopic)
import Service.Group (Group, GroupId)
import Service.MQTT.Class (MQTTClient (..))
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Topic (parseTopic)
import qualified Service.MQTT.Zigbee2MQTT as Zigbee2MQTT
import UnliftIO.Async (Async)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (STM, TChan, TVar, atomically, dupTChan, newBroadcastTChan, newBroadcastTChanIO,
                     newTVar, newTVarIO, writeTChan)

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

  let
    trackNamedSTM :: String -> STM a -> IO a
    trackNamedSTM = \_n v -> atomically v

  daemonBroadcast' <- liftIO . liftIO . trackNamedSTM "daemonBroadcast" $ newBroadcastTChan

  (logger', loggerCleanup) <- mkLogger config'

  subscriptions' <- liftIO . trackNamedSTM "subscriptions" $ newTVar $ defaultTopicActions config' daemonBroadcast'
  (mc, mcCleanup) <- mkMQTTClient config' logger' subscriptions'

  automationBroadcast' <- liftIO . trackNamedSTM "automationBroadcast" $ newBroadcastTChan

  -- 
  --liftIO dumpSTMStats

  -- make stm-stats configurable

  Env config' logger' mc subscriptions' daemonBroadcast' automationBroadcast'
    <$> (liftIO . trackNamedSTM "messageChan" $ dupTChan daemonBroadcast') -- messageChan
    <*> (liftIO . trackNamedSTM "devices" $ newTVar M.empty)                      -- devices
    <*> (liftIO . trackNamedSTM "deviceRegistrations" $ newTVar M.empty)                      -- deviceRegistrations
    <*> (liftIO . trackNamedSTM "groups" $ newTVar M.empty)                      -- groups
    <*> (liftIO . trackNamedSTM "groupRegistrations" $ newTVar M.empty)                      -- groupRegistrations
    <*> (liftIO . trackNamedSTM "scheduledJobs" $ newTVar M.empty)                      -- scheduledJobs
    <*> (liftIO . trackNamedSTM "restartConditions" $ newTVar $
         RestartConditions False False True)    -- restartConditions
    <*> (liftIO . trackNamedSTM "startupMessages" $ newTVar [])                           -- startupMessages
    <*> (liftIO . trackNamedSTM "devicesRawJSON" $ newTVar "")                           -- devicesRawJSON
    <*> (liftIO . trackNamedSTM "groupsRawJSON" $ newTVar "")                           -- groupsRawJSON
    <*> pure (loggerCleanup >> mcCleanup)        -- appCleanup

defaultTopicActions :: Config -> TChan Daemon.Message -> Subscriptions
defaultTopicActions config' daemonBroadcast' =
  let
    (automationServiceTopic', statusTopic') =
      config' ^. mqttConfig . lensProduct automationServiceTopic statusTopic
    setTopic = parseTopic . (<> "/set") . unTopic $ automationServiceTopic'
  in
    M.fromList
      [ ( setTopic -- as in, the topic for setting
        , M.singleton
            Null
            (\_topic msg -> for_ (decode msg) write)
        )

      , ( Zigbee2MQTT.devicesTopic
        , M.singleton
            Null
            (\_topic msg ->
               case decode msg of
                 Just [] -> pure ()
                 Nothing -> pure ()
                 Just devicesJSON ->
                   write . Daemon.DeviceUpdate devicesJSON $ msg
            )
        )

      , ( Zigbee2MQTT.groupsTopic
        , M.singleton
            Null
            (\_topic msg ->
               case decode msg of
                 Just [] -> pure ()
                 Nothing -> pure ()
                 Just groupsJSON ->
                   write . Daemon.GroupUpdate groupsJSON $ msg
            )
        )

      , ( statusTopic'
        , M.singleton Null (\_topic _msg -> write Daemon.Status)
        )
      ]
  where
    write :: Daemon.Message -> IO ()
    write = atomically . writeTChan daemonBroadcast'
