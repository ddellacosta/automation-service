module AutomationService.MQTT
 ( brightness
 , hexColor
 , mkGenericPublishMsg
 , mkPublishMsg
 , publish
 , subscribe
 , state
 )
where

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Prelude (($), (<<<), (<>))

-- helpers for generating MQTT messages to send to devices

type SetState = { state :: String }

state :: String -> SetState
state stateValue = { state: stateValue }

type HexValue = { hex :: String }
type HexColor = { color :: HexValue }

hexColor :: String -> HexColor
hexColor hexValue = { color: { hex: hexValue }}

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

-- not sure how to do this better yet
mkGenericPublishMsg :: String -> String -> String -> String
mkGenericPublishMsg topic propName propVal =
  "{\"topic\": \"" <> topic <>
  "\", \"publish\": \"" <>
  propName <> "\":\"" <>
  propVal <> "\"}"

mkPublishMsg :: forall a. (EncodeJson a) => String -> a -> String
mkPublishMsg topic msg = stringify <<< encodeJson $ publish topic msg
