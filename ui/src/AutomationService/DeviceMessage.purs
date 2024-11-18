module AutomationService.DeviceMessage
 ( Message(..)
 )
where

import Prelude (class Show)

import AutomationService.Device (Device, DeviceId)
import AutomationService.DeviceState (DeviceState)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | LoadDeviceState DeviceState
  | LoadDeviceStateFailed String
  | DeviceSelected DeviceId
  | NoDeviceSelected
  | PublishDeviceMsg String

derive instance Generic Message _

instance Show Message where
  show = genericShow
