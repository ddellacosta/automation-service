module AutomationService.DeviceViewMessage
 ( Message(..)
 )
where

import AutomationService.Device (Device, DeviceId)
import AutomationService.DeviceState (DeviceState)
import Data.Argonaut.Core (Json)

data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | LoadDeviceState DeviceState
  | LoadDeviceStateFailed String
  | DeviceSelected DeviceId
  | NoDeviceSelected
  | PublishDeviceMsg String Json
