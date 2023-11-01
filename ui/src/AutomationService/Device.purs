module AutomationService.Device
  ( Capabilities
  , Device
  , DeviceId
  , Devices
  , decodeDevice
  , decodeDevices
  )
where

import Prelude (bind, pure, show, ($), (<#>), (<>), (<<<))

import AutomationService.Capability (Capability, decodeCapability)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, parseJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, sequence, traverse)

type Capabilities = Array Capability

type DeviceId = String

type Device =
  { id           :: DeviceId
  , name         :: String
  , category     :: String
  , manufacturer :: Maybe String
  , model        :: Maybe String
  , capabilities :: Maybe Capabilities
  }

type Devices = Map DeviceId Device

decodeDevices :: String -> Either JsonDecodeError (Array Device)
decodeDevices jsonStr = do
  devicesBlob <- parseJson jsonStr
  case toArray devicesBlob of
    Just ds -> traverse decodeDevice ds
    Nothing ->
      Left <<< TypeMismatch $ "Expected device array, got " <> show jsonStr

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
