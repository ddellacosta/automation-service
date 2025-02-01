module AutomationService.ResourceMessage
  ( Message(..)
  )
where

import AutomationService.DeviceMessage as Device
import AutomationService.GroupMessage as Group

data Message 
  = DeviceMsg Device.Message
  | GroupMsg Group.Message
  | UpdateCnt
