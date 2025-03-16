module AutomationService.Lighting
  ( ColorSetter(..)
  , getColorSetter
  , getOnOffSwitch
  , getNumericCap
  , getPreset
  , isColor
  , isOnOffSwitch
  , isPreset
  )
where

import AutomationService.Exposes (Capability, CapabilityDetails, Exposes, SubProps(..), capabilityDetails)
import Data.Array (head)
import Data.Array.NonEmpty (mapMaybe, filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Prelude (class Show, ($), (<<<), (>>>), (==))

getOnOffSwitch :: Exposes -> Maybe Capability
getOnOffSwitch exposes =
  head $ filter (isOnOffSwitch <<< capabilityDetails) exposes

isOnOffSwitch :: CapabilityDetails -> Boolean
isOnOffSwitch { subProps } =
  case subProps of
    Binary _props -> true
    _ -> false

getPreset :: String -> Exposes -> Maybe Capability
getPreset presetName exposes =
  head $ filter (isPreset presetName <<< capabilityDetails) exposes

isPreset :: String -> CapabilityDetails -> Boolean
isPreset presetName { name, subProps } =
  case subProps of
    Enum _props -> name == presetName
    _ -> false

getColorSetter :: Exposes -> Maybe ColorSetter
getColorSetter exposes =
  head $ mapMaybe (getColor <<< capabilityDetails) exposes

data ColorSetter
  = HueSatSetter (Array Capability)
  | XYSetter (Array Capability)

derive instance Generic ColorSetter _

instance Show ColorSetter where
  show = genericShow

isColor :: Capability -> Boolean
isColor c = case getColor <<< capabilityDetails $ c of
  Just _ -> true
  Nothing -> false

getColor :: CapabilityDetails -> Maybe ColorSetter
getColor { name, subProps } =
  case name, subProps of
    "color_hs", Composite caps ->
      Just $ HueSatSetter caps

    "color_xy", Composite caps ->
      Just $ XYSetter caps

    _, _ -> Nothing

getNumericCap :: String -> Exposes -> Maybe Capability
getNumericCap capName exposes =
  head $
    filter
      (capabilityDetails >>>
       case _ of
         { name, subProps: Numeric _numProps } ->
           capName == name

         _ ->
           false
      )
      exposes
