
module AutomationService.Device
  ( BinaryProps
  , Capability(..)
  , CapabilityBase
  , Capabilities
  , Device
  , DeviceId
  , Devices
  , EnumProps
  , NumericProps
  , canGet
  , canSet
  , decodeDevice
  , isPublished
  )
where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits ((.&.))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Record (merge)


--
-- access https://www.zigbee2mqtt.io/guide/usage/exposes.html#access
--
isPublished :: Int -> Boolean
isPublished a = 1 .&. a > 0

canSet :: Int -> Boolean
canSet a = 2 .&. a > 0

-- this implies isPublished
canGet :: Int -> Boolean
canGet a = 4 .&. a > 0

type CapabilityBase r =
  { capType :: String
  , featureType :: Maybe String
  , name :: String
  , description :: Maybe String
  , label :: String
  , property :: String
  , access :: Int
  | r
  }

type BinaryProps =
  (valueOn :: String, valueOff :: String, valueToggle :: Maybe String)

type EnumProps = (values :: Array String)

type NumericProps =
  (valueMax :: Maybe Int, valueMin :: Maybe Int, valueStep :: Maybe Int, unit :: Maybe String)

data Capability
  = GenericCap (CapabilityBase ())
  | BinaryCap  (CapabilityBase BinaryProps)
  | EnumCap    (CapabilityBase EnumProps)
  | NumericCap (CapabilityBase NumericProps)

derive instance Generic Capability _

instance Show Capability where
  show = genericShow

decodeCapability :: Maybe String -> Json -> Either JsonDecodeError Capability
decodeCapability featureType capabilityJson = do
  obj <- decodeJson capabilityJson
  capType <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .: "property"
  access <- obj .: "access"
  mValues <- obj .:? "values"
  mValueOn <- obj .:? "value_on"
  mValueOff <- obj .:? "value_off"
  valueToggle <- obj .:? "value_toggle"
  valueMax <- obj .:? "value_max"
  valueMin <- obj .:? "value_min"
  valueStep <- obj .:? "value_step"
  unit <- obj .:? "unit"

  let
    genericCap =
      { capType
      , featureType
      , name
      , description
      , label
      , property
      , access
      }
    hasNumericProp =
      isJust valueMax || isJust valueMin || isJust valueStep || isJust unit

  pure $ case capType, mValues, mValueOn, mValueOff, hasNumericProp of
    "binary", _, Just valueOn, Just valueOff, _ ->
      BinaryCap $ merge genericCap { valueOn, valueOff, valueToggle }
    "enum", Just values, _, _, _ ->
      EnumCap $ merge genericCap { values }
    "numeric", _, _, _, true ->
      NumericCap $ merge genericCap { valueMax, valueMin, valueStep, unit }
    _, _, _, _, _ ->
      GenericCap genericCap

type Capabilities = Array Capability

type DeviceId = String

type Device =
  { id :: DeviceId
  , name :: String
  , category :: String
  , manufacturer :: Maybe String
  , model :: Maybe String
  , capabilities :: Maybe Capabilities
  }

type Devices = Map DeviceId Device

--
-- TODO: make this store failures on as granular a level as possible,
-- only failing at the level a decode fails (vs. now where any
-- failure breaks the entire decoding process), and then report
-- failures back and log
--
decodeDevice :: Json -> Either JsonDecodeError Device
decodeDevice json = do
  obj <- decodeJson json
  id <- obj .: "ieee_address"
  name <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  definition <- obj .:? "definition"
  -- what I've seen at least is that only the Controller has
  -- definition == null. Also see
  -- https://www.zigbee2mqtt.io/guide/usage/mqtt_topics_and_messages.html#zigbee2mqtt-bridge-devices
  capabilities <- for definition decodeCapabilities
  pure { id, name, category, manufacturer, model, capabilities }

  where
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
            Right (Tuple newFeatureType features') ->
              features' <#> decodeCapability (Just newFeatureType)
            _ -> [decodeCapability Nothing e]
        )
        exposes'

    -- when we are dealing with a `features` object, we want to
    -- collect other values to store as we flatten out capabilities
    -- into a single list (for now at least)
    decodeFeatures :: Json -> Either JsonDecodeError (Tuple String (Array Json))
    decodeFeatures exposes = do
      obj <- decodeJson exposes
      features <- obj .:? "features"
      featuresType <- obj .: "type"
      pure $ Tuple featuresType $ fromMaybe [] $ toArray =<< features
