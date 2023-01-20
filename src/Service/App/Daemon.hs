module Service.App.Daemon
  ( run
  )
where

import Control.Lens (view)
import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.UUID.V4 as UUID.V4
import qualified Service.Action as Action
-- TODO need to fix these messaging types to not be garbage
import Service.Action (Action(name, devices, wantsFullControlOver), Message(..), MsgBody (..))
import Service.ActionName (ActionName, serializeActionName)
import Service.Actions (findAction)
import Service.App (ActionsService, Logger(..))
import Service.Device (DeviceId)
import Service.Env (Env, config, loggerCleanup, messagesChan)
import qualified Service.Messages.Action as Messages
import UnliftIO.Async (Async(..), async, cancel)
import UnliftIO.Exception (bracket)
import UnliftIO.STM
  ( TChan
  , TVar
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , newTVarIO
  , readTQueue
  , readTVarIO
  , writeTChan
  , writeTVar
  )

type Action' = Action (TChan Action.Message)

type ThreadMap = M.Map ActionName [(Action', Async ())]

type DeviceMap = M.Map DeviceId [ActionName]

run :: ActionsService ()
run = do
  config' <- view config
  loggerCleanup' <- view loggerCleanup

  bracket (newTVarIO (M.empty :: ThreadMap)) (cleanup loggerCleanup') $ \threadMap -> do
    debug $ T.pack $ show config'

    deviceMap <- newTVarIO (M.empty :: DeviceMap)

    messagesChan' <- view messagesChan

    info "Initializing broadcast and duplicating for server channel"
    broadcastChan <- newBroadcastTChanIO
    serverChan <- atomically $ dupTChan broadcastChan 

    forever $ do
      msg <- atomically $ readTQueue messagesChan'
      debug $ T.pack $ show msg
      case msg of
        Messages.Start actionName ->
          initializeAndRunAction threadMap deviceMap broadcastChan actionName

        Messages.Stop actionName ->
          stopAction threadMap actionName

        Messages.SendTo actionName msg' -> do
          sendClientMsg serverChan $ (serializeActionName actionName) <> msg'

        Messages.Null ->
          debug "Null Action"

  where
    sendClientMsg serverChan = atomically . writeTChan serverChan . Client . MsgBody 

    findThreadsByDeviceId :: [DeviceId] -> ThreadMap -> DeviceMap -> [(Action', Async ())]
    findThreadsByDeviceId devices threadMap' deviceMap' = mconcat $ devices <&> \did ->
      case M.lookup did deviceMap' of
        Just actionNames -> 
          mconcat . catMaybes $ actionNames <&> flip M.lookup threadMap'
        Nothing -> []

    initializeAndRunAction ::
      TVar ThreadMap ->
      TVar DeviceMap ->
      TChan Action.Message ->
      ActionName ->
      ActionsService ()
    initializeAndRunAction threadMap deviceMap broadcastChan actionName = do
      clientChan <- atomically $ dupTChan broadcastChan
      newId <- liftIO UUID.V4.nextRandom
      threadMap' <- readTVarIO threadMap
      deviceMap' <- readTVarIO deviceMap

      let action = findAction actionName newId
          actionsToStop =
            findThreadsByDeviceId (wantsFullControlOver action) threadMap' deviceMap'

      -- shut down devices this action wants a monopoly over, if any
      -- TODO clean these out of the threadMap/deviceMap where they are stored? Or who cares? I can imagine that being costly at high volumes of actions, maybe never an issue...?
      mapM_
        (\(act, asyn) -> do
            debug $
              "Conflicting Device usage: Shutting down threads running action " <>
                (T.pack . show $ name act)
            cancel asyn)
        actionsToStop

      -- start the new action
      clientAsync <-
        async $ runAction (serializeActionName actionName) clientChan action

      atomically $ do
        -- add new entry to ActionName -> (Action, Async ()) map
        writeTVar threadMap $
          M.alter
            (\case
                Nothing -> Just [(action, clientAsync)]
                Just actions -> Just (actions <> [(action, clientAsync)]))
            actionName
            threadMap'

        -- add reference to newly started action in device -> [actionName] map
        mapM_
          (\deviceId -> writeTVar deviceMap $
            M.alter
              (\case
                  Nothing -> Just [actionName]
                  Just actionNames -> Just (actionNames <> [actionName])
              )
              deviceId
              deviceMap')
          (devices action)

    runAction ::
      Text ->
      TChan Action.Message ->
      Action (TChan Action.Message) ->
      ActionsService ()
    runAction
      myName
      broadcastChan
      ( Action.Action _ _ _ _ initAction cleanupAction runAction') =
      bracket
        (initAction myName broadcastChan)
        (cleanupAction myName)
        (runAction' myName)

    stopAction :: TVar ThreadMap -> ActionName -> ActionsService ()
    stopAction threadMap actionName = do
      threadMap' <- readTVarIO threadMap
      for_ (M.lookup actionName threadMap') $
        mapM_ (\(_, async') -> cancel async')

    -- TODO I SHOULD PROBABLY STILL CLEAN THESE UP!
    -- atomically $ writeTVar threadMap $ M.delete actionName threadMap'

    -- final cleanup: if this thing shuts down with threads running,
    -- kill all the threads
    cleanup ::
      (Logger m, MonadIO m, MonadReader Env m, MonadUnliftIO m) =>
      IO () ->
      TVar ThreadMap ->
      m ()
    cleanup loggerCleanup' threadMap = do
      threadMap' <- readTVarIO threadMap
      for_ (M.assocs threadMap') $ \(aName, asyncs) -> do
        info $ "Shutting down Action " <> serializeActionName aName
        mapM_ (\(_, async') -> cancel async') asyncs
      -- TODO does this matter?
      atomically $ writeTVar threadMap M.empty
      liftIO $ loggerCleanup'
