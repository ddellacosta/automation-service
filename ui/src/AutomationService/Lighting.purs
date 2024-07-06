module AutomationService.Lighting
  ( getOnOffSwitch 
  , isOnOffSwitch
  )
where

import AutomationService.Device (DeviceDetails)
import AutomationService.Exposes (Capability, SubProps(..))
import Data.Array (head)
import Data.Array.NonEmpty (filter)
import Data.Maybe (Maybe)
import Prelude (($))

getOnOffSwitch :: DeviceDetails -> Maybe Capability
getOnOffSwitch { exposes } =
  head $ filter isOnOffSwitch exposes

isOnOffSwitch :: Capability -> Boolean
isOnOffSwitch { subProps } = case subProps of
  (Binary _props) -> true 
  _ -> false
