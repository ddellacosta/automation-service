module AutomationService.Device
  ( Device(..)
  , DeviceDetails
  , DeviceId
  , Devices
  , decodeDevice
  , decodeDevices
  , details
  , deviceTopic
  , getTopic
  , setTopic
  )
where

import Prelude (class Show, bind, pure, ($), (<#>), (<>), (<<<))

import AutomationService.Exposes (Exposes, decodeExposes)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, stringify, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, sequence, traverse)

type DeviceId = String

--
-- At some point either I want this to be general enough to support
-- both Matter and Zigbee2MQTT devices, but for now this is basically
-- a Zigbee2MQTT device (as opposed to the main Device type).
---
type DeviceDetails =
  { id           :: DeviceId
  , name         :: String
  , category     :: String
  , manufacturer :: Maybe String
  , model        :: Maybe String
  , exposes      :: Array Exposes
  }

--
-- e.g. One possible model, although it may be better to generalize
-- on some commonly-used top-level properties (id, name, model, maybe
-- more?) and keep the details inside of a property, like exposes
--
-- data DeviceDetails
--   = DeviceDetailsForZigbee Zigbee2MQTTDevice
--   | DeviceDetailsForMatter MatterDevice
--
-- derive instance Generic DeviceDetails _
--
-- instance Show DeviceDetails where
--   show = genericShow
--

data Device
  = OnOffLight DeviceDetails
  | DimmableLight DeviceDetails
  | ColorTemperatureLight DeviceDetails
  | ExtendedColorLight DeviceDetails
  | WindowCovering DeviceDetails

type Devices = Map DeviceId Device

derive instance Generic Device _

instance Show Device where
  show = genericShow

details :: Device -> DeviceDetails
details = case _ of
  OnOffLight d -> d
  DimmableLight d -> d
  ColorTemperatureLight d -> d
  ExtendedColorLight d -> d
  WindowCovering d -> d

decodeDevices :: Json -> Either JsonDecodeError (Array Device)
decodeDevices devicesJson = do
  case toArray devicesJson of
    Just ds -> traverse decodeDevice ds
    Nothing ->
      Left <<< TypeMismatch $ "Expected device array, got " <> stringify devicesJson

--
-- TODO: make this store failures on as granular a level as possible,
-- only failing at the level a decode fails (vs. now where any
-- failure breaks the entire decoding process), and then report
-- failures back and log
--

--   pure $ case isOnOffLight deviceDetails, isCover deviceDetails of
--     true, false -> OnOffLight deviceDetails
--     false, true -> WindowCovering deviceDetails
--     _, _ -> OnOffLight deviceDetails
--
--   where
--     isOnOffLight _deviceDetails = true
--     isCover _deviceDetails = false
--

deviceTopic :: String -> String
deviceTopic name' = "zigbee2mqtt/" <> name'

setTopic :: String -> String
setTopic name' = deviceTopic name' <> "/set"

getTopic :: String -> String
getTopic name' = deviceTopic name' <> "/get"


decodeBaseDevice :: Json -> Array Exposes -> Either JsonDecodeError DeviceDetails
decodeBaseDevice deviceJson exposes = do
  obj <- decodeJson deviceJson
  id' <- obj .: "ieee_address"
  name' <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  pure $ { id: id', name: name', category, manufacturer, model, exposes }

decodeDevice :: Json -> Either JsonDecodeError Device
decodeDevice deviceJson = do
  obj <- decodeJson deviceJson
  --
  -- What I've seen at least is that only the Controller has
  -- definition == null. Also see
  -- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-devices
  --
  mDefinition <- obj .:? "definition"
  exposes <- case mDefinition of
    Just definition -> do
      exposes' <- definition .:? "exposes"
      decodeExposes Nothing (fromMaybe [] exposes')
    Nothing -> pure []
  baseDevice <- decodeBaseDevice deviceJson exposes
  pure $ OnOffLight baseDevice
