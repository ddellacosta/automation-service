module AutomationService.Device
  ( Device(..)
  , DeviceDetails
  , DeviceId
  , Devices
  , _category
  , _deviceDetails
  , _exposes
  , _id
  , _manufacturer
  , _model
  , _name
  , decodeDevice
  , decodeDevices
  , details
  , deviceTopic
  , getTopic
  , setDetails
  , setTopic
  )
where

import AutomationService.Exposes (Exposes, decodeExposes)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, stringify, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (class Show, bind, flip, pure, ($), (<>), (<<<))
import Type.Proxy (Proxy(..))

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
  , exposes      :: Exposes
  }



--
-- lens boilerplate...I miss TemplateHaskell here
--
_id :: forall a b r. Lens { id :: a | r } { id :: b | r } a b
_id = prop (Proxy :: Proxy "id")

_name :: forall a b r. Lens { name :: a | r } { name :: b | r } a b
_name = prop (Proxy :: Proxy "name")

_category :: forall a b r. Lens { category :: a | r } { category :: b | r } a b
_category = prop (Proxy :: Proxy "category")

_manufacturer
  :: forall a b r. Lens { manufacturer :: a | r } { manufacturer :: b | r } a b
_manufacturer = prop (Proxy :: Proxy "manufacturer")

_model :: forall a b r. Lens { model :: a | r } { model :: b | r } a b
_model = prop (Proxy :: Proxy "model")

_exposes :: forall a b r. Lens { exposes :: a | r } { exposes :: b | r } a b
_exposes = prop (Proxy :: Proxy "exposes")


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
--
-- For now the strategy I'm taking is to simplify the device and
-- assume everything is a Zigbee2MQTT device as far as DeviceDetails
-- are concerned, but to generalize the way I reference the
-- capabilities of the Device (which for now just means the stuff
-- inside of DeviceDetails's exposes field, directly from
-- https://www.zigbee2mqtt.io/guide/usage/exposes.html). The Device
-- constructors are modeled after Matter 1.2 spec Device types, and
-- I'll be adding them as I have use for them unless other people
-- start using this for some reason and ask for it (or contribute it
-- themselves).
--
-- In the future I hope that means both Matter devices as well as
-- ESPHome devices.
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

setDetails :: DeviceDetails -> Device -> Device
setDetails d = case _ of
  OnOffLight _ -> OnOffLight d
  DimmableLight _ -> DimmableLight d
  ColorTemperatureLight _ -> ColorTemperatureLight d
  ExtendedColorLight _ -> ExtendedColorLight d
  WindowCovering _ -> WindowCovering d

_deviceDetails :: Lens' Device DeviceDetails
_deviceDetails = lens' $ \d -> Tuple (details d) (flip setDetails d)

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


decodeBaseDevice :: Json -> Exposes -> Either JsonDecodeError DeviceDetails
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
