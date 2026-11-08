module Service.Adapters.DeviceStore
 ( DeltaBatch
 , DeviceMessage(..)
 , DeviceState
 , DeviceStore(..)
 , FrontendMessage(..)
 )
where

import Data.Aeson (ToJSON(..), Value, defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Prelude (Eq, Show)
import Service.Adapters.Capability (Address)
import Service.Adapters.Device (Device(..), DeviceId)
import Service.Adapters.Group (Group, GroupId)

import Network.MQTT.Topic (Topic)

type DeviceState = M.HashMap Address Value

data DeviceStore = DeviceStore
  { devices :: M.HashMap DeviceId Device
  , deviceState :: M.HashMap DeviceId DeviceState
  , groups :: M.HashMap GroupId Group
  } deriving (Eq, Generic, Show)

instance ToJSON DeviceStore where
  toEncoding = genericToEncoding defaultOptions

type DeltaBatch = (DeviceId, [(Address, Value)])

data DeviceMessage
  = DevicesUpdated
  | ESPHomeDeviceUpdated Topic -- temporary
  | GroupsUpdated
  deriving (Eq, Show)

-- move this into its own namespace so names don't collide
data FrontendMessage
  = FDevicesUpdated
  | FGroupsUpdated
  | FDeviceStateUpdated DeltaBatch -- maybe holds deltas?
  | FDeviceCommandReceived DeltaBatch
  deriving (Eq, Show)
