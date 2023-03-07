module Service.App.Daemon
  ( ThreadMap
  , loadDevices -- exported for test use
  , run
  , run' -- exported for test use
  )
where

import Prelude hiding (filter)

import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value, decode)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Service.Automation as Automation
import Service.Automation (Automation, Message(..))
import Service.AutomationName (AutomationName, serializeAutomationName)
import Service.Automations (findAutomation)
import Service.App (Logger(..), MonadMQTT)
import qualified Service.Device as Device
import Service.Device (Device, DeviceId)
import Service.Env
  ( Env
  , appCleanup
  , automationBroadcast
  , config
  , deviceRegistrations
  , devices
  , messageChan
  , mqttDispatch
  , serverChan
  )
import qualified Service.Messages.Daemon as Daemon
import Service.Messages.Daemon (AutomationSchedule)
import System.Cron (addJob, execSchedule)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Exception (bracket, finally)
import UnliftIO.STM
  ( STM
  , TChan
  , TVar
  , atomically
  , dupTChan
  , modifyTVar'
  , newTVarIO
  , readTChan
  , readTVar
  , stateTVar
  , writeTChan
  , writeTVar
  )

type AutomationEntry m = (Automation m, Async ())

type ThreadMap m = M.Map AutomationName (AutomationEntry m)

run
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => m ()
run = do
  threadMapTV <- newTVarIO M.empty
  run' threadMapTV

--
-- Splitting this from the above is a bit of a hack to enable easily
-- testing threadMapTV without needing to include it in Env, which
-- brings the Monad m type variable into play, which makes it a pain
-- to deal with. Anyways ThreadMap is really only a concern of this
-- module anyways, so it's probably for the best regardless.
--
run'
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> m ()
run' threadMapTV = do
  config' <- view config
  appCleanup' <- view appCleanup

  flip finally (cleanupAutomations appCleanup' threadMapTV) $ do
    debug . T.pack . show $ config'
    go

  where
    go = do
      messageChan' <- view messageChan
      msg <- atomically $ readTChan messageChan'

      debug $ "Received Message in main Daemon thread: " <> T.pack (show msg)

      case msg of
        Daemon.Start automationName ->
          initializeAndRunAutomation threadMapTV automationName *> go

        Daemon.Stop automationName ->
          stopAutomation threadMapTV automationName *> go

        Daemon.SendTo automationName msg' -> do
          serverChan' <- view serverChan
          sendClientMsg automationName serverChan' msg' *> go

        Daemon.Schedule automationMessage automationSchedule ->
          addScheduleAutomationMessage
            automationMessage automationSchedule messageChan' *> go

        Daemon.DeviceUpdate devices' -> do
          storedDevices <- view devices
          loadDevices storedDevices devices' *> go

        Daemon.Register deviceId automationName ->
          addRegisteredDevice messageChan' deviceId automationName *> go

        Daemon.Subscribe mTopic automationBroadcastChan ->
          for_ mTopic $ \topic -> do
            mqttDispatch' <- view mqttDispatch
            atomically . modifyTVar' mqttDispatch' $
              M.insertWith (<>) topic $
                mkDefaultTopicMsgAction automationBroadcastChan :| []

        Daemon.Null -> debug "Null Automation" *> go

    mkDefaultTopicMsgAction automationBroadcastChan = \topicMsg ->
      for_ (decode topicMsg) $ atomically . writeTChan automationBroadcastChan

initializeAndRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => TVar (ThreadMap m)
  -> AutomationName
  -> m ()
initializeAndRunAutomation
  threadMapTV automationName = do
    automationBroadcast' <- view automationBroadcast
    clientChan <- atomically $ dupTChan automationBroadcast'

    let automation = findAutomation automationName

    clientAsync <- async $
      bracket (pure clientChan) (Automation._cleanup automation) (Automation._run automation)

    atomically $
      insertAutomation threadMapTV automationName (automation, clientAsync)

  where
    --
    -- Given a TVar ThreadMap and a (AutomationName, (Automation m, Async ()))
    -- pair, inserts a new entry into the ThreadMap. If the ThreadMap
    -- already contains a List of (Automation m, Async ()) pairs at that
    -- index it will append a new one to the end of that List, otherwise
    -- it will add a new List with the new entry as its first member.
    --
    insertAutomation :: TVar (ThreadMap m) -> AutomationName -> AutomationEntry m -> STM ()
    insertAutomation threadMapTV' automationName' automationEntry = do
      threadMap' <- readTVar threadMapTV'
      writeTVar threadMapTV' $ M.insert automationName' automationEntry threadMap'

stopAutomation
  :: (Logger m, MonadUnliftIO m) => TVar (ThreadMap m) -> AutomationName -> m ()
stopAutomation threadMapTV automationName = do
  threadMap <- atomically . readTVar $ threadMapTV
  info $ "Shutting down Automation " <> serializeAutomationName automationName
  for_ (M.lookup automationName threadMap) $ \(_, async') -> cancel async'
  atomically . writeTVar threadMapTV . M.delete automationName $ threadMap

cleanupAutomations
  :: (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m)
  => IO ()
  -> TVar (ThreadMap m)
  -> m ()
cleanupAutomations appCleanup' threadMapTV = do
  threadMap <- atomically . readTVar $ threadMapTV
  for_ (M.assocs threadMap) $ \(automationName, (_, async')) -> do
    info $ "Shutting down Automation " <> serializeAutomationName automationName
    cancel async'
  liftIO appCleanup'

sendClientMsg
  :: (MonadUnliftIO m) => AutomationName -> TChan Message -> Value -> m ()
sendClientMsg automationName serverChan' =
  atomically . writeTChan serverChan' . Client automationName

addScheduleAutomationMessage
  :: (MonadIO m)
  => Daemon.Message
  -> AutomationSchedule
  -> TChan Daemon.Message
  -> m ()
addScheduleAutomationMessage automationMessage automationSchedule messageChan' = do
  void . liftIO . execSchedule $ flip addJob automationSchedule $
    atomically . writeTChan messageChan' $ automationMessage

loadDevices
  :: (MonadUnliftIO m) => TVar (Map DeviceId Device) -> [Device] -> m ()
loadDevices storedDevices devices' =
  atomically . writeTVar storedDevices . M.fromList $
    (\d -> (Device._id d, d)) <$> devices'

addRegisteredDevice
  :: (MonadReader Env m, MonadUnliftIO m)
  => TChan Daemon.Message
  -> DeviceId
  -> AutomationName
  -> m ()
addRegisteredDevice daemonBroadcast' deviceId newAutoName = do
  deviceRegs <- view deviceRegistrations
  atomically $ do
    mPrevAutoName <- stateTVar deviceRegs $ \deviceRegs' ->
      (M.lookup deviceId deviceRegs', M.insert deviceId newAutoName deviceRegs')
    flip (maybe $ pure ()) mPrevAutoName $ \prevAutoName ->
      writeTChan daemonBroadcast' $ Daemon.Stop prevAutoName
