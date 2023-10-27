module AutomationService.DeviceViewMessage
 ( Message(..)
 )
where

import AutomationService.Device (Device, DeviceId)

data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | DeviceSelected DeviceId
  | PublishDeviceMsg String String
