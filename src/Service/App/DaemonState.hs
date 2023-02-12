{-# LANGUAGE TemplateHaskell #-}

module Service.App.DaemonState
  ( ActionEntry
  , DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , broadcastChan
  , deviceMap
  , initDaemonState
  , insertAction
  , insertDeviceActions
  , removeActions
  , threadMap
  , serverChan
  )
  where

import Control.Lens (makeFieldsNoPrefix)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import Service.Action (Action, Message)
import Service.ActionName (ActionName)
import Service.Device (DeviceId)
import UnliftIO.Async (Async)
import UnliftIO.STM
  ( STM
  , TChan
  , TVar
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , newTVarIO
  , readTVar
  , writeTVar
  )

data ServerResponse
  = MsgLoopEnd
  | StoppingServer
  deriving (Show)

type ActionEntry m = (Action m, Async ())

type ThreadMap m = M.Map ActionName [ActionEntry m]

type DeviceMap = M.Map DeviceId [ActionName]

data DaemonState m = DaemonState
  { _threadMap :: TVar (ThreadMap m)
  , _deviceMap :: TVar DeviceMap
  , _broadcastChan :: TChan Message
  , _serverChan :: TChan Message
  }

makeFieldsNoPrefix ''DaemonState

initDaemonState :: IO (DaemonState m)
initDaemonState = do
  broadcastChan' <- newBroadcastTChanIO
  serverChan' <- atomically $ dupTChan broadcastChan'
  threadMap' <- newTVarIO M.empty
  deviceMap' <- newTVarIO M.empty
  pure $ DaemonState threadMap' deviceMap' broadcastChan' serverChan'


-- |
-- | Given a TVar ThreadMap and a (ActionName, (Action m, Async ()))
-- | pair, inserts a new entry into the ThreadMap. If the ThreadMap
-- | already contains a List of (Action m, Async ()) pairs at that
-- | index it will append a new one to the end of that List, otherwise
-- | it will add a new List with the new entry as its first member.
-- |
insertAction :: TVar (ThreadMap m) -> ActionName -> ActionEntry m -> STM ()
insertAction threadMap' actionName actionEntry = do
  threadMap'' <- readTVar threadMap'
  writeTVar threadMap' $
    M.alter (Just . foldr (<>) [actionEntry]) actionName threadMap''

removeActions :: TVar (ThreadMap m) -> [ActionName] -> STM ()
removeActions threadMap' actions =
  readTVar threadMap' >>=
    \tm -> writeTVar threadMap' $ foldr M.delete tm actions

-- |
-- | Given a TVar DeviceMap, this will add a new ActionName entry for
-- | all matching DeviceIds passed in.
-- |
insertDeviceActions :: TVar DeviceMap -> [DeviceId] -> ActionName -> STM ()
insertDeviceActions deviceMap' devices actionName = do
  deviceMap'' <- readTVar deviceMap'
  for_ devices $ \deviceId ->
    writeTVar deviceMap' $
      M.alter (Just . foldr (<>) [actionName]) deviceId deviceMap''
