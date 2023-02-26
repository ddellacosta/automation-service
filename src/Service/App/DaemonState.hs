{-# LANGUAGE TemplateHaskell #-}

module Service.App.DaemonState
  ( AutomationEntry
  , DaemonState(..)
  , DeviceMap
  , ServerResponse(..)
  , ThreadMap
  , broadcastChan
  , deviceMap
  , findThreadsByDeviceId
  , initDaemonState
  , insertAutomation
  , insertDeviceAutomation
  , removeAutomations
  , removeDeviceAutomations
  , serverChan
  , threadMap
  )
  where

import Control.Lens ((<&>), makeFieldsNoPrefix)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, toList)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Service.Automation as Automation
import Service.Automation (Automation)
import Service.AutomationName (AutomationName)
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

type AutomationEntry m = (Automation m, Async ())

type ThreadMap m = M.Map AutomationName (AutomationEntry m)

type DeviceMap = M.Map DeviceId (NonEmpty AutomationName)

data DaemonState m = DaemonState
  { _threadMap :: TVar (ThreadMap m)
  , _deviceMap :: TVar DeviceMap
  , _broadcastChan :: TChan Automation.Message
  , _serverChan :: TChan Automation.Message
  }

makeFieldsNoPrefix ''DaemonState

initDaemonState :: IO (DaemonState m)
initDaemonState = do
  broadcastChan' <- newBroadcastTChanIO
  serverChan' <- atomically $ dupTChan broadcastChan'
  threadMapTV <- newTVarIO M.empty
  deviceMapTV <- newTVarIO M.empty
  pure $ DaemonState threadMapTV deviceMapTV broadcastChan' serverChan'


-- | Given a TVar ThreadMap and a (AutomationName, (Automation m, Async ()))
-- pair, inserts a new entry into the ThreadMap. If the ThreadMap
-- already contains a List of (Automation m, Async ()) pairs at that
-- index it will append a new one to the end of that List, otherwise
-- it will add a new List with the new entry as its first member.
--
insertAutomation :: TVar (ThreadMap m) -> AutomationName -> AutomationEntry m -> STM ()
insertAutomation threadMapTV automationName automationEntry = do
  threadMap' <- readTVar threadMapTV
  writeTVar threadMapTV $ M.insert automationName automationEntry threadMap'


-- | Removes the Automations corresponding to the list of AutomationNames
--  passed in from the TVar-wrapped ThreadMap.
--
removeAutomations :: TVar (ThreadMap m) -> [AutomationName] -> STM ()
removeAutomations threadMapTV automations =
  readTVar threadMapTV >>=
    \tm -> writeTVar threadMapTV $ foldr M.delete tm automations


-- | Given a TVar DeviceMap, this will add a new AutomationName entry for
-- all matching DeviceIds passed in.
--
insertDeviceAutomation :: TVar DeviceMap -> [DeviceId] -> AutomationName -> STM ()
insertDeviceAutomation deviceMapTV devices automationName = do
  deviceMap' <- readTVar deviceMapTV
  for_ devices $ \deviceId ->
    writeTVar deviceMapTV $
      M.alter (Just . foldr (<>) (automationName :| [])) deviceId deviceMap'


-- | Removes all AutomationNames from any Device entries in the TVar
-- DeviceMap.
--
removeDeviceAutomations :: TVar DeviceMap -> NonEmpty AutomationName -> STM ()
removeDeviceAutomations deviceMapTV automationNames = do
  deviceMap' <- readTVar deviceMapTV
  for_ (M.keys deviceMap') $ \deviceId ->
    writeTVar deviceMapTV $
      M.alter (filterAutomationNames =<<) deviceId deviceMap'
  where
    filterAutomationNames =
      nonEmpty . flip (foldr (\an -> filter (/= an))) automationNames . toList


-- | A utility for extracting any AutomationEntries using any of the
-- Devices identified by DeviceIds passed in as the first
-- argument. Used for figuring out what threads to kill when a newly
-- started Automation wants sole ownership over one or more devices.
--
findThreadsByDeviceId :: [DeviceId] -> ThreadMap m -> DeviceMap -> [(Automation m, Async ())]
findThreadsByDeviceId devices' threadMap' deviceMap' = mconcat $ devices' <&> \did ->
  case M.lookup did deviceMap' of
    Just automationNames -> mapMaybe (`M.lookup` threadMap') $ toList automationNames
    Nothing -> []
