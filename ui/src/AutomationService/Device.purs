module AutomationService.Device
  ( Device(..)
  , DeviceDetails
  , DeviceId
  , Devices
  , DeviceType(..)
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
  , deviceType
  , getTopic
  , setDetails
  , setTopic
  )
where

import AutomationService.Exposes (Capability, CapType(..), Exposes, FeatureType(..), decodeExposes)
import Control.Alt ((<|>))
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Array (filter, null)
import Data.Array.NonEmpty (fromArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..), isRight)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Ord, class Show, bind, flip, not, pure, (<<<), ($), (<$>), (<>), (=<<), (==), (&&), (||))
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
-- constructors are modeled after Matter 1.4 spec Device types, and
-- I'll be adding them as I have use for them unless other people
-- start using this for some reason and ask for it (or contribute it
-- themselves).
--
-- In the future I hope that means both Matter devices as well as
-- ESPHome devices, however that could work.
--

--
-- This is just a flat list of all Matter device types, added as I
-- get around to it/am motivated to do so by wanting to use a given
-- device/on the off chance someone else actually wants to make a PR.
--
-- The Ord instance means that these are ordered in reverse order
-- according to feature set of any given group of items, with the
-- assumption being made that there will be no meaningful overlap
-- between features across device sets (e.g. Lights vs. Sensors vs.
-- Switches vs. etc.) such that if we identify multiple possible
-- Device types based on what comes back in Exposes, a sort will
-- always pop the most feature-full type to the top...but this may
-- only work with lights...not sure yet. Other device types seem to
-- have basically completely disjoint sets of distinguishing
-- properties.
--
-- In any case, a flat list like this has seemed to be the easiest to
-- work with so far. Having a lot of structure here is hard to
-- work with and maintain, and is not reflected in the Matter spec
-- itself in any case.
--
data Device
  = ExtendedColorLight DeviceDetails
  | ColorTemperatureLight DeviceDetails
  | DimmableLight DeviceDetails
  | OnOffLight DeviceDetails
  | GenericSwitch DeviceDetails
  | ContactSensor DeviceDetails
  | OccupancySensor DeviceDetails
  | HumiditySensor DeviceDetails
  | TemperatureSensor DeviceDetails
  | AirQualitySensor DeviceDetails
  | WindowCovering DeviceDetails
  | UnknownDevice DeviceDetails

type Devices = Map DeviceId Device

derive instance Eq Device
derive instance Generic Device _
derive instance Ord Device

instance Show Device where
  show = genericShow

details :: Device -> DeviceDetails
details = case _ of
  ExtendedColorLight d -> d
  ColorTemperatureLight d -> d
  DimmableLight d -> d
  OnOffLight d -> d
  GenericSwitch d -> d
  ContactSensor d -> d
  OccupancySensor d -> d
  HumiditySensor d -> d
  TemperatureSensor d -> d
  AirQualitySensor d -> d
  WindowCovering d -> d
  UnknownDevice d -> d

setDetails :: DeviceDetails -> Device -> Device
setDetails d = case _ of
  ExtendedColorLight _ -> ExtendedColorLight d
  ColorTemperatureLight _ -> ColorTemperatureLight d
  DimmableLight _ -> DimmableLight d
  OnOffLight _ -> OnOffLight d
  GenericSwitch _ -> GenericSwitch d
  ContactSensor _ -> ContactSensor d
  OccupancySensor _ -> OccupancySensor d
  HumiditySensor _ -> HumiditySensor d
  TemperatureSensor _ -> TemperatureSensor d
  AirQualitySensor _ -> AirQualitySensor d
  WindowCovering _ -> WindowCovering d
  UnknownDevice _ -> UnknownDevice d

_deviceDetails :: Lens' Device DeviceDetails
_deviceDetails = lens' $ \d -> Tuple (details d) (flip setDetails d)

-- an experiment, mostly for tests at this point
data DeviceType
  = ExtendedColorLight'
  | ColorTemperatureLight'
  | DimmableLight'
  | OnOffLight'
  | GenericSwitch'
  | ContactSensor'
  | OccupancySensor'
  | HumiditySensor'
  | TemperatureSensor'
  | AirQualitySensor'
  | WindowCovering'
  | UnknownDevice'

derive instance Eq DeviceType
derive instance Generic DeviceType _
derive instance Ord DeviceType

instance Show DeviceType where
  show = genericShow

deviceType :: Device -> DeviceType
deviceType = case _ of
  ExtendedColorLight _ -> ExtendedColorLight'
  ColorTemperatureLight _ -> ColorTemperatureLight'
  DimmableLight _ -> DimmableLight'
  OnOffLight _ -> OnOffLight'
  GenericSwitch _ -> GenericSwitch'
  ContactSensor _ -> ContactSensor'
  OccupancySensor _ -> OccupancySensor'
  HumiditySensor _ -> HumiditySensor'
  TemperatureSensor _ -> TemperatureSensor'
  AirQualitySensor _ -> AirQualitySensor'
  WindowCovering _ -> WindowCovering'
  UnknownDevice _ -> UnknownDevice'

-- okay I guess these go in here too

deviceTopic :: String -> String
deviceTopic name' = "zigbee2mqtt/" <> name'

setTopic :: String -> String
setTopic name' = deviceTopic name' <> "/set"

getTopic :: String -> String
getTopic name' = deviceTopic name' <> "/get"

--

decodeDevices :: Json -> Either JsonDecodeError (Array Device)
decodeDevices devicesJson = do
  case toArray devicesJson of
    Just ds ->
      -- REPORT THESE FAILURES! Either isn't really enough for what I want here?
      case (filter isRight $ decodeDevice <$> ds) of
        [] -> Left (UnexpectedValue devicesJson)
        ds' -> sequence ds'
    Nothing ->
      Left (UnexpectedValue devicesJson)

decodeBaseDevice :: Json -> Exposes -> Either JsonDecodeError DeviceDetails
decodeBaseDevice deviceJson exposes = do
  obj <- decodeJson deviceJson
  id' <- obj .: "ieee_address"
  name' <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  pure $ { id: id', name: name', category, manufacturer, model, exposes }

hasCapabilities :: Exposes -> (Capability -> Boolean) -> Boolean
hasCapabilities exposes pred =
    not <<< null <<< NonEmpty.filter pred $ exposes

mkDefaultDevice :: DeviceDetails -> Either JsonDecodeError Device
mkDefaultDevice = Right <<< UnknownDevice

--
-- Matter Device Library Specification R1.4
-- Chapter 4. Lighting Device Types
--
decodeLightDevice :: DeviceDetails -> Either JsonDecodeError Device
decodeLightDevice baseDevice =
  let
    defaultDevice = mkDefaultDevice baseDevice

    extendedColorLight =
      if hasCapabilities baseDevice.exposes $ \e ->
        (e.name == "color_xy" || e.name == "hue")
        && e.type == Composite'
      then
        Right <<< ExtendedColorLight $ baseDevice
      else
        Left <<< TypeMismatch $ "Not an ExtendedColorLight"

    colorTemperatureLight =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "color_temp" && e.type == Numeric'
      then
        Right <<< ColorTemperatureLight $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a ColorTemperatureLight"

    dimmableLight =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "brightness" && e.type == Numeric'
      then
        Right <<< DimmableLight $ baseDevice
      else
        Left <<< TypeMismatch $ "Not an DimmableLight"

    onOffLight =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.type == Binary'
      then
        Right <<< OnOffLight $ baseDevice
      else
        Left <<< TypeMismatch $ "Not an OnOffLight"

  in
   --
   -- It's probably worth noting that this actually depends on the
   -- order that these are checked in (unlike other device subtype
   -- decoding functions which are essentially all just based on
   -- distinct predicate checks), as each lighting type tends to
   -- subsume all the properties of the lighting types underneath it
   -- in the hierarchy. E.g. all lights have an on/off switch
   -- property, and all lights have brightness from DimmableLight
   -- "and up."
   --
   extendedColorLight
     <|> colorTemperatureLight
     <|> dimmableLight
     <|> onOffLight
     <|> defaultDevice


--
-- Matter Device Library Specification R1.4
-- Chapter 6. Switches and Controls Device Types
--
decodeControlsDevice :: DeviceDetails -> Either JsonDecodeError Device
decodeControlsDevice baseDevice =
  let
    defaultDevice = mkDefaultDevice baseDevice

    --
    -- unimplemented:
    --
    -- OnOffLightSwitch
    -- DimmerSwitch
    -- ColorDimmerSwitch
    -- ControlBridge
    -- PumpController
    --

    genericSwitch =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "action" && e.type == Enum'
      then
        Right <<< GenericSwitch $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a GenericSwitch"

  in
   genericSwitch <|> defaultDevice


--
-- Matter Device Library Specification R1.4
-- Chapter 7. Sensor Device Types
--
decodeSensorDevice :: DeviceDetails -> Either JsonDecodeError Device
decodeSensorDevice baseDevice =
  let
    defaultDevice = mkDefaultDevice baseDevice

    --
    -- unimplemented:
    --
    -- LightSensor
    -- PressureSensor
    -- FlowSensor
    -- OnOffSensor
    -- SmokeCOAlarm
    -- WaterFreezeDetector
    -- WaterLeakDetector
    -- RainSensor
    --

    contactSensor =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "contact" && e.type == Binary'
      then
        Right <<< ContactSensor $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a ContactSensor"

    occupancySensor =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "occupancy" && e.type == Binary'
      then
        Right <<< OccupancySensor $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a OccupancySensor"

    humiditySensor =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "humidity" && e.type == Numeric'
      then
        Right <<< HumiditySensor $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a HumiditySensor"

    temperatureSensor =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "temperature" && e.type == Numeric'
      then
        Right <<< TemperatureSensor $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a TemperatureSensor"

    airQualitySensor =
      if hasCapabilities baseDevice.exposes $ \e ->
        -- Not really sure which of these is "correct", so just
        -- checking for them both in any case since they both fit the
        -- definition:
        (e.name == "air_quality" && e.type == Enum')
        || (e.name == "voc" && e.type == Numeric')
      then
        Right <<< AirQualitySensor $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a AirQualitySensor"

  in
    contactSensor
      <|> occupancySensor
      -- This is one case where the ordering matters; most humidity
      -- sensors are also temperature sensors, and air quality sensors
      -- include both as well. So if we choose the wrong one here we may
      -- miss out on being able to dispatch the most
      -- capability-inclusive, not that we can't remedy that to some
      -- extent in UI based on dynamic capability checking, but that is
      -- less ideal (otherwise why are we using PureScript):
      <|> airQualitySensor
      <|> humiditySensor
      <|> temperatureSensor
      <|> defaultDevice


--
-- Matter Device Library Specification R1.4
-- Chapter 8. Closure Device Types
--
decodeClosureDevice :: DeviceDetails -> Either JsonDecodeError Device
decodeClosureDevice baseDevice =
  let
    defaultDevice = mkDefaultDevice baseDevice

    --
    -- unimplemented:
    --
    -- DoorLock
    -- DoorLockController
    -- WindowCoveringController
    --

    windowCovering =
      if hasCapabilities baseDevice.exposes $ \e ->
        e.name == "position" && e.type == Numeric'
      then
        Right <<< WindowCovering $ baseDevice
      else
        Left <<< TypeMismatch $ "Not a WindowCovering"

  in
   windowCovering <|> defaultDevice


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
      exposes <- definition .:? "exposes"
      case fromArray =<< exposes of
        Just exposes' -> decodeExposes Nothing exposes'
        _ -> Left (TypeMismatch $ "Empty exposes list")
    Nothing -> Left (TypeMismatch $ "No definition")

  baseDevice <- decodeBaseDevice deviceJson exposes

  let
    defaultDevice = mkDefaultDevice baseDevice

    lightDevice =
      if hasCapabilities exposes $ \e ->
        e.featureType == Just Light
      then
        decodeLightDevice baseDevice
      else
        Left <<< TypeMismatch $ "Not a Light Device"

    controlsDevice =
      if hasCapabilities exposes $ \e ->
        e.name == "action" && e.type == Enum'
      then
        decodeControlsDevice baseDevice
      else
        Left <<< TypeMismatch $ "Not a Controls Device"

    --
    -- This one has no specific featureType so I have to do it this
    -- ass-backwards way where I delineate all the specific sensor
    -- type predicates and then do it again inside of
    -- decodeSensorDevice, but this feels cleaner to me even if it's
    -- less efficient. If it seems to cause performance issues I'll
    -- do it the ugly...er, even uglier way ¯\_(ツ)_/¯
    --
    sensorDevice =
      if hasCapabilities exposes $ \e ->
           e.name == "contact" && e.type == Binary'      -- ContactSensor
        || e.name == "occupancy" && e.type == Binary'    -- OccupancySensor
        || e.name == "temperature" && e.type == Numeric' -- TemperatureSensor
        || e.name == "humidity" && e.type == Numeric'    -- HumiditySensor
        || e.name == "air_quality" && e.type == Enum'    -- AirQualitySensor
        || e.name == "voc" && e.type == Numeric'         -- AirQualitySensor
      then
        decodeSensorDevice baseDevice
      else
        Left <<< TypeMismatch $ "Not a Sensor Device"

    --
    -- The only one of these I have to test with right now is the
    -- IKEA window blinds, so not sure if they will all have this
    -- featureType...but probably, if we can assume things behave
    -- like lights do wrt featureType being consistent?
    --
    closureDevice =
      if hasCapabilities exposes $ \e ->
        e.featureType == Just Cover
      then
        decodeClosureDevice baseDevice
      else
        Left <<< TypeMismatch $ "Not a Closure Device"

  lightDevice
    <|> controlsDevice
    <|> sensorDevice
    <|> closureDevice
    <|> defaultDevice
