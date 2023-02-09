module Service.App.Daemon
  ( DeviceMap
  , ThreadMap
  , initializeAndRunAction
  , run
  )
where

import Control.Lens (view)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Service.Action as Action
import Service.Action
  ( Action
  , ActionFor(name, devices, wantsFullControlOver)
  , Message(..)
  , MsgBody (..)
  )
import Service.ActionName (ActionName, serializeActionName)
import Service.Actions (findAction)
import Service.App (Logger(..), MonadMQTT)
import Service.App.Helpers (findThreadsByDeviceId)
import Service.App.DaemonState
  ( DaemonState(..)
  , DeviceMap
  , ThreadMap
  , insertAction
  , insertDeviceActions
  )
import Service.Env (Env, config, appCleanup, messagesChan)
import qualified Service.Messages.Action as Messages
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (bracket)
import UnliftIO.STM
  ( TVar
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , newTVarIO
  , readTQueue
  , readTVarIO
  , writeTChan
  , writeTVar
  )


run :: (Logger m, MonadReader Env m, MonadMQTT m, MonadUnliftIO m) => m ()
run = do
  config' <- view config
  appCleanup' <- view appCleanup

  bracket (newTVarIO (M.empty :: ThreadMap m)) (cleanupActions appCleanup') $
    \threadMap -> do
      (debug . T.pack . show) config'

      deviceMap <- newTVarIO (M.empty :: DeviceMap)
      broadcastChan <- newBroadcastTChanIO
      serverChan <- atomically $ dupTChan broadcastChan

      let
        daemonState = DaemonState threadMap deviceMap broadcastChan serverChan

      messagesChan' <- view messagesChan

      forever $ do
        msg <- atomically $ readTQueue messagesChan'
        debug $ T.pack $ show msg
        dispatchAction daemonState msg


dispatchAction
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => DaemonState m
  -> Messages.Action Text
  -> m ()
dispatchAction
  daemonState@(DaemonState _threadMap _deviceMap _broadcastChan _serverChan) = \case
    Messages.Start actionName ->
      initializeAndRunAction daemonState actionName findAction

    Messages.Stop actionName ->
      stopAction _threadMap actionName

    Messages.SendTo actionName msg' -> do
      sendClientMsg _serverChan $ serializeActionName actionName <> msg'

    Messages.Null ->
      debug "Null Action"

    where
      sendClientMsg serverChan' = atomically . writeTChan serverChan' . Client . MsgBody


initializeAndRunAction
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
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
  :: (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m)
  => IO ()
  -> TVar (ThreadMap m)
  -> m ()
cleanupActions appCleanup' threadMap = do
  threadMap' <- readTVarIO threadMap
  for_ (M.assocs threadMap') $ \(aName, asyncs) -> do
    info $ "Shutting down Action " <> serializeActionName aName
    mapM_ (\(_, async') -> cancel async') asyncs
  -- TODO does this matter?
  atomically $ writeTVar threadMap M.empty
  liftIO appCleanup'
