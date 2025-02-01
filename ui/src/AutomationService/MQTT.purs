module AutomationService.MQTT
 ( brightness
 , hexColor
 , hslColor
 , mkGenericPublishMsg
 , mkPublishMsg
 , publish
 , subscribe
 , state
 )
where

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Data.Int (floor)
import Prelude (($), (<<<), (<>), (*))

-- helpers for generating MQTT messages to send to devices

type SetState = { state :: String }

state :: String -> SetState
state stateValue = { state: stateValue }

type HexValue = { hex :: String }
type HexColor = { color :: HexValue }

-- type HSLValue = { h :: Int, s :: Int, l :: Int }
type HSLValue = { hue :: Int, saturation :: Int }
type HSLColor = { color :: HSLValue }

hexColor :: String -> HexColor
hexColor hexValue = { color: { hex: hexValue }}

hslColor :: forall r. { h :: Number, s :: Number | r } -> HSLColor
hslColor { h, s } =
  { color:
    { hue: floor h
    , saturation: floor s
    }
  }

type Brightness = { brightness :: String }

brightness :: String -> Brightness
brightness = { brightness: _ }

type PublishMsg a =
  { topic :: String
  , publish :: a
  }

publish :: forall a. String -> a -> PublishMsg a
publish topic msg = { topic, publish: msg }

type SubscribeMsg =
  { subscribe :: String -- automation name; this should be predefined
  , topic :: String
  }

subscribe :: String -> String -> SubscribeMsg
subscribe topic autoName = { subscribe: autoName, topic }

--
-- experiments in generating a record dynamically, via Record.insert
--
-- import Data.Reflectable (class Reflectable, class Reifiable, reifyType)
-- import Record as Record
-- import Type.Proxy (Proxy(..))
--

-- how do I generate a new Record, including a key name generated
-- from a string, at runtime, in PureScript?
mkGenericPublishMsg :: String -> String -> String -> String
mkGenericPublishMsg topic propName propVal =
  "{\"topic\": \"" <> topic <>
  "\", \"publish\": {\"" <>
  propName <> "\":\"" <>
  propVal <> "\"}}"

mkPublishMsg :: forall a. (EncodeJson a) => String -> a -> String
mkPublishMsg topic msg = stringify <<< encodeJson $ publish topic msg
