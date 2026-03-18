module AutomationService.DeviceMessage
 ( Message(..)
 )
where

import Prelude (class Show, ($), (<<<), (<>), show)

import AutomationService.Device (Devices, DeviceId)
import AutomationService.DeviceState (DeviceState)
import Data.Argonaut (Json, toString)
import Data.Generic.Rep (class Generic)
-- No type class instance was found for
-- Data.Show.Show Json
-- import Data.Show.Generic (genericShow)

data Message
  = LoadDevices Devices
  | LoadDevicesFailed String
  | LoadDeviceState DeviceState
  | LoadDeviceStateFailed String
  | DeviceSelected DeviceId
  | NoDeviceSelected
  | PublishDeviceMsg String
  -- group messages
  | LoadGroupsFailed String
  | LoadGroups Json
  | ReLoadGroups
  | PublishGroupMsg String

derive instance Generic Message _

instance Show Message where
  show = case _ of
    LoadGroups groupsJson ->
      "LoadGroups " <> (show <<< toString $ groupsJson)
    msg ->
      show msg
