module AutomationService.MQTT
 ( brightness
 , genericProp
 , hexColor
 , publish
 , subscribe
 , state
 )
where

import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Prelude ((<>))

-- JSON helpers for generating MQTT messages to send to devices

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

type PublishMsg =
  { topic :: String
  , publish :: Json
  }

publish :: String -> Json -> PublishMsg
publish topic msg = { topic, publish: msg }

type SubscribeMsg =
  { subscribe :: String -- automation name; this should be predefined
  , topic :: String
  }

subscribe :: String -> String -> SubscribeMsg
subscribe topic autoName = { subscribe: autoName, topic }

-- not sure how to do this better yet
genericProp :: String -> String -> Json
genericProp propName propVal = encodeJson parsed
  where
    jsonStr = "{\"" <> propName <> "\":\"" <> propVal <> "\"}"

    parsed = case jsonParser jsonStr of
      Right parsed' -> parsed'
      Left _fail -> jsonNull
