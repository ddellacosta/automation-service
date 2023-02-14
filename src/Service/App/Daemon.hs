module Service.App.Daemon
  ( DeviceMap
  , ThreadMap
  , run
  , run'
  )
where

import Prelude hiding (filter)

import Control.Lens (view)
import Control.Monad (forM_)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Service.Action as Action
import Service.Action
  ( Action(name, devices, wantsFullControlOver)
  , Message(..)
  )
import Service.ActionName (ActionName, serializeActionName)
import Service.Actions (findAction)
import Service.App (Logger(..), MonadMQTT)
import Service.App.DaemonState
  ( DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , findThreadsByDeviceId
  , initDaemonState
  , insertAction
  , insertDeviceAction
  , removeActions
  , removeDeviceActions
  )
import Service.Env (Env', config, appCleanup, messageQueue)
import qualified Service.Messages.Action as Messages
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (bracket, bracket_)
import UnliftIO.STM
  ( TChan
  , TQueue
  , TVar
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
  run' daemonState findAction responseQueue

run'
  :: (Logger m, MonadReader (Env' logger mqttClient) m, MonadMQTT m, MonadUnliftIO m)
  => DaemonState m
  -> (ActionName -> Action m)
  -> TQueue ServerResponse -- see note below about writing to responseQueue
  -> m ()
run' daemonState findAction' responseQueue = do
  config' <- view config
  appCleanup' <- view appCleanup

  bracket_ (pure ()) (cleanupActions appCleanup' daemonState) $ do
    debug . T.pack . show $ config'
    go findAction'

  where
    go findAction'' = do
      messageQueue' <- view messageQueue
      msg <- atomically $ readTQueue messageQueue'
      debug $ "Received Message in main Daemon thread: " <> T.pack (show msg)

      case msg of
        Messages.StopServer -> do
          -- see note below about writing to responseQueue
          atomically $ writeTQueue responseQueue StoppingServer
          pure ()

        Messages.Start actionName -> runServerStep findAction'' $
          initializeAndRunAction daemonState actionName findAction''

        Messages.Stop actionName -> runServerStep findAction'' $
          stopAction (_threadMap daemonState) (_deviceMap daemonState) actionName

        Messages.SendTo actionName msg' -> runServerStep findAction'' $
          sendClientMsg actionName (_serverChan daemonState) msg'

        Messages.Schedule _actionName _actionSchedule ->
          runServerStep findAction'' $
            pure ()

        Messages.Null -> runServerStep findAction'' $
          debug "Null Action"

    runServerStep findAction'' step = do
      step
      -- To be honest this is just here to allow me to write
      -- integration tests that exercise this entire daemon process
      -- while it's running in a separate thread. If there was another
      -- way to ensure that I can block on checking assertions while
      -- running this process in a thread, I'd get rid of this,
      -- because it doesn't actually provide any functionality in a
      -- production context.
      atomically $ writeTQueue responseQueue MsgLoopEnd
      go findAction''

    sendClientMsg
      :: (MonadUnliftIO m) => ActionName -> TChan Message -> Value -> m ()
    sendClientMsg actionName serverChan' =
      atomically . writeTChan serverChan' . Client actionName

initializeAndRunAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => DaemonState m
  -> ActionName
  -> (ActionName -> Action m)
  -> m ()
initializeAndRunAction
  (DaemonState _threadMap _deviceMap _broadcastChan _) actionName findAction' = do
    clientChan <- atomically $ dupTChan _broadcastChan
    threadMap' <- readTVarIO _threadMap
    deviceMap' <- readTVarIO _deviceMap

    let
      action = findAction' actionName
      actionsToStop =
        findThreadsByDeviceId (wantsFullControlOver action) threadMap' deviceMap'

    mapM_ stopAction' actionsToStop
    atomically $ do
      let stopped = name . fst <$> actionsToStop
      removeActions _threadMap stopped
      forM_ (nonEmpty stopped) $ \stopped' ->
        removeDeviceActions _deviceMap stopped'

    clientAsync <- async $
      bracket (pure clientChan) (Action.cleanup action) (Action.run action)

    atomically $ do
      insertAction _threadMap actionName (action, clientAsync)
      insertDeviceAction _deviceMap (devices action) actionName

    where
      stopAction' (act, asyn) = do
        debug $ "Conflicting Device usage: Shutting down threads running action "
          <> (T.pack . show $ name act)
        cancel asyn

stopAction :: (MonadUnliftIO m) => TVar (ThreadMap m) -> TVar DeviceMap -> ActionName -> m ()
stopAction threadMap deviceMap actionName = do
  threadMap' <- readTVarIO threadMap
  for_ (M.lookup actionName threadMap') $
    mapM_ (\(_, async') -> cancel async')
  atomically $ do
    writeTVar threadMap $ M.delete actionName threadMap'
    removeDeviceActions deviceMap (actionName :| [])

cleanupActions
  :: (Logger m, MonadIO m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => IO ()
  -> DaemonState m
  -> m ()
cleanupActions appCleanup' daemonState = do
  let threadMap = _threadMap daemonState
  threadMap' <- readTVarIO threadMap
  for_ (M.assocs threadMap') $ \(aName, asyncs) -> do
    info $ "Shutting down Action " <> serializeActionName aName
    mapM_ (\(_, async') -> cancel async') asyncs
  liftIO appCleanup'
