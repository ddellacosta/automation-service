module Service.App.Daemon
  ( DeviceMap
  , ThreadMap
  , run
  , run'
  )
where

import Prelude hiding (filter)

import Control.Lens ((^.), view)
import Control.Lens.Unsound (lensProduct)
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value)
import Data.Foldable (for_, forM_)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Service.Automation as Automation
import Service.Automation
  ( Automation(_name)
  , Message(..)
  , name
  , wantsFullControlOver
  )
import Service.AutomationName (AutomationName(LuaScript), serializeAutomationName)
import Service.Automations (findAutomation)
import Service.Automations.LuaScript (mkLuaAutomation)
import Service.App (Logger(..), MonadMQTT)
import Service.App.DaemonState
  ( DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , deviceMap
  , findThreadsByDeviceId
  , initDaemonState
  , insertAutomation
  , insertDeviceAutomation
  , removeAutomations
  , removeDeviceAutomations
  , threadMap
  )
import Service.Env (Env, config, devices, appCleanup, messageQueue)
import qualified Service.Messages.Daemon as Daemon
import Service.Messages.Daemon (AutomationSchedule)
import System.Cron (addJob, execSchedule)
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (bracket, bracket_)
import UnliftIO.STM
  ( TChan
  , TQueue
  , atomically
  , dupTChan
  , newTQueueIO
  , readTQueue
  , readTVarIO
  , writeTChan
  , writeTQueue
  , writeTVar
  )


run
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => m ()
run = do
  daemonState <- liftIO initDaemonState
  responseQueue <- newTQueueIO
  run' daemonState responseQueue
  -- -- only once the Daemon is running do we want to trigger a call to zigbee2mqtt to load the devices:
  -- publish Zigbee2MQTTDevice.topic 

--
-- Splitting this from `run` is a hack to allow me to test the main
-- logic more easily. I suppose I could move the initialization above
-- to Main but it feels more natural to wrap it in this module, and by
-- the same token I don't feel like DaemonState should be stored in
-- Env.
--
run'
  :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m)
  => DaemonState m
  -> TQueue ServerResponse -- see note below about writing to responseQueue
  -> m ()
run' daemonState responseQueue = do
  config' <- view config
  appCleanup' <- view appCleanup

  bracket_ (pure ()) (cleanupAutomations appCleanup' daemonState) $ do
    debug . T.pack . show $ config'
    go

  where
    go = do
      messageQueue' <- view messageQueue
      storedDevices <- view devices
      msg <- atomically $ readTQueue messageQueue'
      debug $ "Received Message in main Daemon thread: " <> T.pack (show msg)

      case msg of
        Daemon.StopServer -> do
          -- see note below about writing to responseQueue
          atomically $ writeTQueue responseQueue StoppingServer

        Daemon.StartLua filePath -> runServerAction $
          initializeAndRunLuaScriptAutomation daemonState LuaScript filePath

        Daemon.Start automationName -> runServerAction $
          initializeAndRunAutomation daemonState automationName

        Daemon.Stop automationName -> runServerAction $
          stopAutomation daemonState automationName

        Daemon.SendTo automationName msg' -> runServerAction $
          sendClientMsg automationName (_serverChan daemonState) msg'

        Daemon.Schedule automationMessage automationSchedule -> runServerAction $ do
          addScheduleAutomationMessage
            automationMessage automationSchedule messageQueue'

        Daemon.DeviceUpdate devices' -> runServerAction $ do
          atomically $ writeTVar storedDevices devices'

        Daemon.Null -> runServerAction $
          debug "Null Automation"

    runServerAction action = do
      action
      -- To be honest this is just here to allow me to write
      -- integration tests that exercise this entire daemon process
      -- while it's running in a separate thread. If there was another
      -- way to ensure that I can block on checking assertions while
      -- running this process in a thread, I'd get rid of this,
      -- because it doesn't actually provide any functionality in a
      -- production context.
      atomically $ writeTQueue responseQueue MsgLoopEnd
      go

sendClientMsg
  :: (MonadUnliftIO m) => AutomationName -> TChan Message -> Value -> m ()
sendClientMsg automationName serverChan' =
  atomically . writeTChan serverChan' . Client automationName

addScheduleAutomationMessage
  :: (MonadIO m)
  => Daemon.Message
  -> AutomationSchedule
  -> TQueue Daemon.Message
  -> m ()
addScheduleAutomationMessage automationMessage automationSchedule messageQueue' = do
  void $ liftIO $ execSchedule $ flip addJob automationSchedule $ do
    atomically $ writeTQueue messageQueue' automationMessage

initializeAndRunAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => DaemonState m
  -> AutomationName
  -> m ()
initializeAndRunAutomation daemonState automationName =
  initializeAndRunAutomation' daemonState automationName Nothing

initializeAndRunLuaScriptAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => DaemonState m
  -> AutomationName
  -> FilePath
  -> m ()
initializeAndRunLuaScriptAutomation daemonState automationName =
  initializeAndRunAutomation' daemonState automationName . Just

initializeAndRunAutomation'
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => DaemonState m
  -> AutomationName
  -> Maybe FilePath
  -> m ()
initializeAndRunAutomation'
  (DaemonState threadMapTV deviceMapTV broadcastChan' _) automationName mFilePath = do
    clientChan <- atomically $ dupTChan broadcastChan'
    threadMap' <- readTVarIO threadMapTV
    deviceMap' <- readTVarIO deviceMapTV

    let
      automation = maybe (findAutomation automationName) mkLuaAutomation mFilePath
      automationsToStop =
        findThreadsByDeviceId (automation ^. wantsFullControlOver) threadMap' deviceMap'

    mapM_ stopAutomation' automationsToStop
    atomically $ do
      let stopped = _name . fst <$> automationsToStop
      removeAutomations threadMapTV stopped
      forM_ (nonEmpty stopped) $ \stopped' ->
        removeDeviceAutomations deviceMapTV stopped'

    clientAsync <- async $
      bracket (pure clientChan) (Automation._cleanup automation) (Automation._run automation)

    atomically $ do
      insertAutomation threadMapTV automationName (automation, clientAsync)
      insertDeviceAutomation deviceMapTV (automation ^. Automation.devices) automationName

    where
      stopAutomation' (act, asyn) = do
        debug $ "Conflicting Device usage: Shutting down threads running automation "
          <> (T.pack . show $ act ^. name)
        cancel asyn

stopAutomation :: (MonadUnliftIO m) => DaemonState m -> AutomationName -> m ()
stopAutomation daemonState automationName = do
  let
    (threadMapTV, deviceMapTV) = daemonState ^. lensProduct threadMap deviceMap
  threadMap' <- readTVarIO threadMapTV
  for_ (M.lookup automationName threadMap') $ \(_, async') -> cancel async'
  atomically $ do
    writeTVar threadMapTV $ M.delete automationName threadMap'
    removeDeviceAutomations deviceMapTV (automationName :| [])

cleanupAutomations
  :: (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m)
  => IO ()
  -> DaemonState m
  -> m ()
cleanupAutomations appCleanup' daemonState = do
  let threadMapTV = daemonState ^. threadMap
  threadMap' <- readTVarIO threadMapTV
  for_ (M.assocs threadMap') $ \(automationName, (_, async')) -> do
    info $ "Shutting down Automation " <> serializeAutomationName automationName
    cancel async'
  liftIO appCleanup'
