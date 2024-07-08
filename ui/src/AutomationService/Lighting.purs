module AutomationService.Lighting
  ( ColorSetter(..)
  , getColorSetter
  , getOnOffSwitch
  , getPreset
  , isColor
  , isOnOffSwitch
  , isPreset
  )
where

import AutomationService.Device (DeviceDetails)
import AutomationService.Exposes (Capability, SubProps(..))
import Data.Array (head)
import Data.Array.NonEmpty (mapMaybe, filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Prelude (class Show, ($), (==))

getOnOffSwitch :: DeviceDetails -> Maybe Capability
getOnOffSwitch { exposes } =
  head $ filter isOnOffSwitch exposes

isOnOffSwitch :: Capability -> Boolean
isOnOffSwitch { subProps } =
  case subProps of
    Binary _props -> true
    _ -> false

getPreset :: String -> DeviceDetails -> Maybe Capability
getPreset presetName { exposes } =
  head $ filter (isPreset presetName) exposes

isPreset :: String -> Capability -> Boolean
isPreset presetName { name, subProps } =
  case subProps of
    Enum _props -> name == presetName
    _ -> false

getColorSetter :: DeviceDetails -> Maybe ColorSetter
getColorSetter { exposes } =
  head $ mapMaybe getColor exposes

data ColorSetter
  = HueSatSetter (Array Capability)
  | XYSetter (Array Capability)

derive instance Generic ColorSetter _

instance Show ColorSetter where
  show = genericShow

isColor :: Capability -> Boolean
isColor c = case getColor c of
  Just _ -> true
  Nothing -> false

getColor :: Capability -> Maybe ColorSetter
getColor { name, subProps } =
  case name, subProps of
    "color_hs", Composite caps ->
      Just $ HueSatSetter caps

    "color_xy", Composite caps ->
      Just $ XYSetter caps

    _, _ -> Nothing
