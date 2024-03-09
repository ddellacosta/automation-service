module AutomationService.Device
  ( Capabilities
  , CoverType(..)
  , Device(..)
  , DeviceDetails(..)
  , DeviceId
  , Devices
  , LightType(..)
  , Zigbee2MQTTDevice
  , decodeDevice
  , decodeDevices
  , deviceTopic
  , getTopic
  , id
  , name
  , setTopic
  )
where

import Prelude (class Show, bind, pure, ($), (<#>), (<>), (<<<))

import AutomationService.Capability (Capability, decodeCapability)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, stringify, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, sequence, traverse)

type Capabilities = Array Capability

type DeviceId = String

type Zigbee2MQTTDevice =
  { id           :: DeviceId
  , name         :: String
  , category     :: String
  , manufacturer :: Maybe String
  , model        :: Maybe String
  , capabilities :: Maybe Capabilities
  }

data DeviceDetails
  = DeviceDetailsForZigbee Zigbee2MQTTDevice
  | EmptyDetails
  -- | DeviceDetailsForMatter MatterDevice

derive instance Generic DeviceDetails _

instance Show DeviceDetails where
  show = genericShow

data LightType
  = OnOffLight
  | DimmableLight
  | ColorTemperatureLight
  | ExtendedColorLight

derive instance Generic LightType _

instance Show LightType where
  show = genericShow

data CoverType
  = WindowBlind

derive instance Generic CoverType _

instance Show CoverType where
  show = genericShow

data Device
  = Light LightType DeviceDetails
  | Cover CoverType DeviceDetails

type Devices = Map DeviceId Device

derive instance Generic Device _

instance Show Device where
  show = genericShow

details :: Device -> DeviceDetails
details = case _ of
  Light _ d -> d
  Cover _ d -> d
  -- _ -> EmptyDetails

name :: Device -> String
name d = case details d of
  (DeviceDetailsForZigbee d') -> d'.name
  _ -> "No name"

id :: Device -> String
id d = case details d of
  (DeviceDetailsForZigbee d') -> d'.id
  _ -> "No name"

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
decodeDevice :: Json -> Either JsonDecodeError Device
decodeDevice json = do
  obj <- decodeJson json
  id' <- obj .: "ieee_address"
  name' <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  definition <- obj .:? "definition"
  -- what I've seen at least is that only the Controller has
  -- definition == null. Also see
  -- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-devices
  capabilities <- for definition decodeCapabilities
  let
    deviceDetails =
      DeviceDetailsForZigbee
      { id: id'
      , name: name'
      , category
      , manufacturer
      , model
      , capabilities
      }
  pure $ case isOnOffLight deviceDetails, isCover deviceDetails of
    true, false -> Light OnOffLight $ deviceDetails
    false, true -> Cover WindowBlind $ deviceDetails
    _, _ -> Light OnOffLight $ deviceDetails

  where
    isOnOffLight _deviceDetails = true
    isCover _deviceDetails = false

    decodeCapabilities :: Json -> Either JsonDecodeError Capabilities
    decodeCapabilities definition = do
      obj <- decodeJson definition
      exposes <- obj .: "exposes"
      let
        exposes' = fromMaybe [] $ toArray exposes
      -- this feels a bit complicated
      -- see the section at the top here for some of the explanation
      -- why https://www.zigbee2mqtt.io/guide/usage/exposes.html
      sequence $ foldMap
        (\e ->
          case decodeFeatures e of
            Right { featureType, features } ->
              features <#> decodeCapability (Just featureType)
            _ -> [decodeCapability Nothing e]
        )
        exposes'

    -- when we are dealing with a `features` object, we want to
    -- collect other values to store as we flatten out capabilities
    -- into a single list (for now at least)
    decodeFeatures
      :: Json
      -> Either JsonDecodeError { featureType :: String, features :: Array Json }
    decodeFeatures exposes = do
      obj <- decodeJson exposes
      features <- obj .: "features"
      featureType <- obj .: "type"
      pure $ { featureType, features: fromMaybe [] $ toArray features }

deviceTopic :: String -> String
deviceTopic name' = "zigbee2mqtt/" <> name'

setTopic :: String -> String
setTopic name' = deviceTopic name' <> "/set"

getTopic :: String -> String
getTopic name' = deviceTopic name' <> "/get"
