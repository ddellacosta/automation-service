module AutomationService.DeviceViewMessage
 ( Message(..)
 )
where

import AutomationService.Device (Device, DeviceId)
import AutomationService.DeviceState (DeviceState)

data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | LoadDeviceState DeviceState
  | LoadDeviceStateFailed String
  | DeviceSelected DeviceId
  | PublishDeviceMsg String String
