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
import qualified Service.Action as Action
import Service.Action
  ( Action(_name)
  , Message(..)
  , devices
  , name
  , wantsFullControlOver
  )
import Service.ActionName (ActionName(LuaScript), serializeActionName)
import Service.Actions (findAction)
import Service.Actions.LuaScript (mkLuaAction)
import Service.App (Logger(..), MonadMQTT)
import Service.App.DaemonState
  ( DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , deviceMap
  , findThreadsByDeviceId
  , initDaemonState
  , insertAction
  , insertDeviceAction
  , removeActions
  , removeDeviceActions
  , threadMap
  )
import Service.Env (Env', config, appCleanup, messageQueue)
import qualified Service.Messages.Action as Messages
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
  :: (Logger m, MonadReader (Env' logger mqttClient) m, MonadMQTT m, MonadUnliftIO m)
  => m ()
run = do
  daemonState <- liftIO initDaemonState
  responseQueue <- newTQueueIO
  run' daemonState responseQueue

--
-- Splitting this from `run` is a hack to allow me to test the main
-- logic more easily. I suppose I could move the initialization above
-- to Main but it feels more natural to wrap it in this module, and by
-- the same token I don't feel like DaemonState should be stored in
-- Env.
--
run'
  :: (Logger m, MonadReader (Env' logger mqttClient) m, MonadMQTT m, MonadUnliftIO m)
  => DaemonState m
  -> TQueue ServerResponse -- see note below about writing to responseQueue
  -> m ()
run' daemonState responseQueue = do
  config' <- view config
  appCleanup' <- view appCleanup

  bracket_ (pure ()) (cleanupActions appCleanup' daemonState) $ do
    debug . T.pack . show $ config'
    go

  where
    go = do
      messageQueue' <- view messageQueue
      msg <- atomically $ readTQueue messageQueue'
      debug $ "Received Message in main Daemon thread: " <> T.pack (show msg)

      case msg of
        Messages.StopServer -> do
          -- see note below about writing to responseQueue
          atomically $ writeTQueue responseQueue StoppingServer
          pure ()

        Messages.StartLua filePath -> runServerStep $
          initializeAndRunLuaScriptAction daemonState LuaScript filePath

        Messages.Start actionName -> runServerStep $
          initializeAndRunAction daemonState actionName

        Messages.Stop actionName -> runServerStep $
          stopAction daemonState actionName

        Messages.SendTo actionName msg' -> runServerStep $
          sendClientMsg actionName (_serverChan daemonState) msg'

        Messages.Schedule actionMessage actionSchedule -> runServerStep $ do
          addScheduleActionMessage
            actionMessage actionSchedule messageQueue'

          pure ()

        Messages.Null -> runServerStep $
          debug "Null Action"

    runServerStep step = do
      step
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
  :: (MonadUnliftIO m) => ActionName -> TChan Message -> Value -> m ()
sendClientMsg actionName serverChan' =
  atomically . writeTChan serverChan' . Client actionName

addScheduleActionMessage
  :: (MonadIO m)
  => Messages.Action
  -> Messages.ActionSchedule
  -> TQueue Messages.Action
  -> m ()
addScheduleActionMessage actionMessage actionSchedule messageQueue' = do
  void $ liftIO $ execSchedule $ flip addJob actionSchedule $ do
    atomically $ writeTQueue messageQueue' actionMessage

initializeAndRunAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => DaemonState m
  -> ActionName
  -> m ()
initializeAndRunAction daemonState actionName =
  initializeAndRunAction' daemonState actionName Nothing

initializeAndRunLuaScriptAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => DaemonState m
  -> ActionName
  -> FilePath
  -> m ()
initializeAndRunLuaScriptAction daemonState actionName =
  initializeAndRunAction' daemonState actionName . Just

initializeAndRunAction'
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => DaemonState m
  -> ActionName
  -> Maybe FilePath
  -> m ()
initializeAndRunAction'
  (DaemonState threadMapTV deviceMapTV broadcastChan' _) actionName mFilePath = do
    clientChan <- atomically $ dupTChan broadcastChan'
    threadMap' <- readTVarIO threadMapTV
    deviceMap' <- readTVarIO deviceMapTV

    let
      action = maybe (findAction actionName) mkLuaAction mFilePath
      actionsToStop =
        findThreadsByDeviceId (action ^. wantsFullControlOver) threadMap' deviceMap'

    mapM_ stopAction' actionsToStop
    atomically $ do
      let stopped = _name . fst <$> actionsToStop
      removeActions threadMapTV stopped
      forM_ (nonEmpty stopped) $ \stopped' ->
        removeDeviceActions deviceMapTV stopped'

    clientAsync <- async $
      bracket (pure clientChan) (Action._cleanup action) (Action._run action)

    atomically $ do
      insertAction threadMapTV actionName (action, clientAsync)
      insertDeviceAction deviceMapTV (action ^. devices) actionName

    where
      stopAction' (act, asyn) = do
        debug $ "Conflicting Device usage: Shutting down threads running action "
          <> (T.pack . show $ act ^. name)
        cancel asyn

stopAction :: (MonadUnliftIO m) => DaemonState m -> ActionName -> m ()
stopAction daemonState actionName = do
  let
    (threadMapTV, deviceMapTV) = daemonState ^. lensProduct threadMap deviceMap
  threadMap' <- readTVarIO threadMapTV
  for_ (M.lookup actionName threadMap') $ \(_, async') -> cancel async'
  atomically $ do
    writeTVar threadMapTV $ M.delete actionName threadMap'
    removeDeviceActions deviceMapTV (actionName :| [])

cleanupActions
  :: (Logger m, MonadIO m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => IO ()
  -> DaemonState m
  -> m ()
cleanupActions appCleanup' daemonState = do
  let threadMapTV = daemonState ^. threadMap
  threadMap' <- readTVarIO threadMapTV
  for_ (M.assocs threadMap') $ \(actionName, (_, async')) -> do
    info $ "Shutting down Action " <> serializeActionName actionName
    cancel async'
  liftIO appCleanup'
