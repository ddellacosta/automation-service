{-# LANGUAGE TemplateHaskell #-}

module Service.App.DaemonState
  ( DaemonState(..)
  , broadcastChan
  , deviceMap
  , threadMap
  , serverChan
  )
  where

import Control.Lens (makeFieldsNoPrefix)
import Service.Action (Message)
import Service.App.DeviceMap (DeviceMap)
import Service.App.ThreadMap (ThreadMap)
import UnliftIO.STM (TChan, TVar)

data DaemonState m = DaemonState
  { _threadMap :: TVar (ThreadMap m)
  , _deviceMap :: TVar DeviceMap
  , _broadcastChan :: TChan Message
  , _serverChan :: TChan Message
  }

makeFieldsNoPrefix ''DaemonState
