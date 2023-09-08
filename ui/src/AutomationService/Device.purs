
module AutomationService.Device
  ( Capability
  , Capabilities
  , Device
  , DeviceId
  , Devices
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
import Data.Int.Bits ((.&.))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))


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

type Capability =
  { capType :: String
  , featureType :: Maybe String
  , name :: String
  , description :: Maybe String
  , label :: String
  , property :: String
  , access :: Int
  }

decodeCapability :: Json -> Either JsonDecodeError Capability
decodeCapability capabilityJson = do
  obj <- decodeJson capabilityJson
  capType <- obj .: "type"
  name <- obj .: "name"
  description <- obj .:? "description"
  label <- obj .: "label"
  property <- obj .: "property"
  access <- obj .: "access"
  pure { capType, featureType: Nothing, name, description, label, property, access }

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
              features' <#>
                decodeCapability >>> map _ { featureType = Just newFeatureType }
            _ -> [decodeCapability e]
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
