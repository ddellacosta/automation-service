module Service.App.Daemon
  ( DeviceMap
  , ThreadMap
  , run
  , run'
  )
where

import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Service.Action as Action
import Service.Action
  ( Action
  , ActionFor(name, devices, wantsFullControlOver)
  , Message(..)
  )
import Service.ActionName (ActionName, serializeActionName)
import Service.Actions (findAction)
import Service.App (Logger(..), MonadMQTT)
import Service.App.Helpers (findThreadsByDeviceId)
import Service.App.DaemonState
  ( DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , initDaemonState
  , insertAction
  , insertDeviceActions
  , removeActions
  )
import Service.Env (Env', config, appCleanup, messagesQueue)
import qualified Service.Messages.Action as Messages
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (bracket, bracket_)
import UnliftIO.STM
  ( TQueue
  , TVar
  , atomically
  , dupTChan
  , newTQueueIO
  , readTQueue
  , readTVarIO
  , writeTChan
  , writeTQueue
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

  bracket_(pure ()) (cleanupActions appCleanup' daemonState) $ do
    (debug . T.pack . show) config'
    messagesQueue' <- view messagesQueue
    go messagesQueue' findAction'

  where
    go messagesQueue' findAction'' = do
      msg <- atomically $ readTQueue messagesQueue'
      debug $ T.pack $ show msg

      case msg of
        Messages.StopServer -> do
          -- see note below about writing to responseQueue
          atomically $ writeTQueue responseQueue StoppingServer
          pure ()

        Messages.Start actionName -> runServerStep messagesQueue' findAction'' $
          initializeAndRunAction daemonState actionName findAction''

        Messages.Stop actionName -> runServerStep messagesQueue' findAction'' $
          stopAction (_threadMap daemonState) actionName

        Messages.SendTo actionName msg' -> runServerStep messagesQueue' findAction'' $
          sendClientMsg (_serverChan daemonState) $ serializeActionName actionName <> msg'

        Messages.Null -> runServerStep messagesQueue' findAction'' $
          debug "Null Action"

    runServerStep messagesQueue' findAction'' step = do
      step
      -- To be honest this is just here to allow me to write
      -- integration tests that exercise this entire daemon process
      -- while it's running in a separate thread. If there was another
      -- way to ensure that I can block on checking assertions while
      -- running this process in a thread, I'd get rid of this,
      -- because it doesn't actually provide any functionality in a
      -- production context.
      atomically $ writeTQueue responseQueue MsgLoopEnd
      go messagesQueue' findAction''

    sendClientMsg serverChan' = atomically . writeTChan serverChan' . Client


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
    atomically $ removeActions _threadMap $ name . fst <$> actionsToStop

    clientAsync <- async $
      bracket (pure clientChan) (Action.cleanup action) (Action.run action)

    atomically $ do
      insertAction _threadMap actionName (action, clientAsync)
      insertDeviceActions _deviceMap (devices action) actionName

    where
      stopAction' (act, asyn) = do
        debug $ "Conflicting Device usage: Shutting down threads running action "
          <> (T.pack . show $ name act)
        cancel asyn


stopAction :: (MonadUnliftIO m) => TVar (ThreadMap m) -> ActionName -> m ()
stopAction threadMap actionName = do
  threadMap' <- readTVarIO threadMap
  for_ (M.lookup actionName threadMap') $
    mapM_ (\(_, async') -> cancel async')

-- TODO I SHOULD PROBABLY STILL CLEAN THESE UP!
-- atomically $ writeTVar threadMap $ M.delete actionName threadMap'


-- final cleanup: if this thing shuts down with threads running,
-- kill all the threads
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
