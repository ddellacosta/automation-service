{-# LANGUAGE TemplateHaskell #-}

module Service.App.DaemonState
  ( ActionEntry
  , DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , broadcastChan
  , deviceMap
  , findThreadsByDeviceId
  , initDaemonState
  , insertAction
  , insertDeviceAction
  , removeActions
  , removeDeviceActions
  , serverChan
  , threadMap
  )
  where

import Control.Lens ((<&>), makeFieldsNoPrefix)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
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

type ThreadMap m = M.Map ActionName (ActionEntry m)

type DeviceMap = M.Map DeviceId (NonEmpty ActionName)

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
  threadMapTV <- newTVarIO M.empty
  deviceMapTV <- newTVarIO M.empty
  pure $ DaemonState threadMapTV deviceMapTV broadcastChan' serverChan'

-- |
-- | Given a TVar ThreadMap and a (ActionName, (Action m, Async ()))
-- | pair, inserts a new entry into the ThreadMap. If the ThreadMap
-- | already contains a List of (Action m, Async ()) pairs at that
-- | index it will append a new one to the end of that List, otherwise
-- | it will add a new List with the new entry as its first member.
-- |
insertAction :: TVar (ThreadMap m) -> ActionName -> ActionEntry m -> STM ()
insertAction threadMapTV actionName actionEntry = do
  threadMap' <- readTVar threadMapTV
  writeTVar threadMapTV $ M.insert actionName actionEntry threadMap'

-- |
-- | Removes the Actions corresponding to the list of ActionNames
-- | passed in from the TVar-wrapped ThreadMap.
-- |
removeActions :: TVar (ThreadMap m) -> [ActionName] -> STM ()
removeActions threadMapTV actions =
  readTVar threadMapTV >>=
    \tm -> writeTVar threadMapTV $ foldr M.delete tm actions

-- |
-- | Given a TVar DeviceMap, this will add a new ActionName entry for
-- | all matching DeviceIds passed in.
-- |
insertDeviceAction :: TVar DeviceMap -> [DeviceId] -> ActionName -> STM ()
insertDeviceAction deviceMapTV devices actionName = do
  deviceMap' <- readTVar deviceMapTV
  for_ devices $ \deviceId ->
    writeTVar deviceMapTV $
      M.alter (Just . foldr (<>) (actionName :| [])) deviceId deviceMap'

-- |
-- | Removes all ActionNames from any Device entries in the TVar
-- | DeviceMap.
-- |
removeDeviceActions :: TVar DeviceMap -> NonEmpty ActionName -> STM ()
removeDeviceActions deviceMapTV actionNames = do
  deviceMap' <- readTVar deviceMapTV
  for_ (M.keys deviceMap') $ \deviceId ->
    writeTVar deviceMapTV $
      M.alter (filterActionNames =<<) deviceId deviceMap'
  where
    filterActionNames =
      nonEmpty . flip (foldr (\an -> filter (/= an))) actionNames . toList

-- |
-- | A utility for extracting any ActionEntries using any of the
-- | Devices identified by DeviceIds passed in as the first
-- | argument. Used for figuring out what threads to kill when a newly
-- | started Action wants sole ownership over one or more devices.
-- |
findThreadsByDeviceId :: [DeviceId] -> ThreadMap m -> DeviceMap -> [(Action m, Async ())]
findThreadsByDeviceId devices' threadMap' deviceMap' = mconcat $ devices' <&> \did ->
  case M.lookup did deviceMap' of
    Just actionNames -> mapMaybe (`M.lookup` threadMap') $ toList actionNames
    Nothing -> []
