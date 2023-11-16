module AutomationService.DeviceState
 ( Color
 , DeviceState
 , DeviceStates
 , DeviceSummary
 , Update
 , decodeDeviceState
 )
where

import Prelude

import AutomationService.Device (DeviceId)
import Control.Alternative ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeNumber, decodeString)
import Data.Either (Either (..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Traversable (for, traverse)

type DeviceStates = Map DeviceId DeviceState

type Color =
  { x :: Number
  , y :: Number
  , h :: Maybe Int
  , s :: Maybe Int
  , hue :: Maybe Int
  , saturation :: Maybe Int
  }

decodeColor :: Json -> Either JsonDecodeError Color
decodeColor colorJson = do
  obj <- decodeJson colorJson
  x <- obj .: "x"
  y <- obj .: "y"
  h <- obj .:? "h"
  s <- obj .:? "s"
  hue <- obj .:? "hue"
  saturation <- obj .:? "saturation"
  pure { x, y, h, s, hue, saturation }


--
-- these should match those in Device and use same subtypes where
-- possible (deviceType == category)
--
type DeviceSummary =
  { applicationVersion :: Maybe Int
  , dateCode :: String
  , friendlyName :: String
  , hardwareVersion :: Either Number String
  , ieeeAddr :: String
  , manufacturerId :: Int
  , manufacturerName :: String
  , model :: String
  , networkAddress :: Int
  , powerSource :: Maybe String
  , softwareBuildId :: Maybe String
  , stackVersion :: Maybe Int
  , deviceType :: String -- should be enum, matching category in Device
  , zclVersion :: Maybe Int
  }

decodeDeviceSummary :: Json -> Either JsonDecodeError DeviceSummary
decodeDeviceSummary deviceSummaryJson = do
  obj <- decodeJson deviceSummaryJson
  applicationVersion <- obj .:? "applicationVersion"
  dateCode <- obj .: "dateCode"
  friendlyName <- obj .: "friendlyName"
  hardwareVersion <- obj .: "hardwareVersion" >>= \obj' ->
    --
    -- I didn't think I would have to implement this, but something
    -- about the DecodeJson instance for Either fails at runtime
    -- with
    --   LoadDeviceStateFailed with msg: (AtKey "hardwareVersion" \
    --     (Named "Either" (TypeMismatch "Object")))
    --
    -- ...maybe I'm misunderstanding how the Either instance works?
    --
    Left <$> decodeNumber obj' <|> Right <$> decodeString obj'
  ieeeAddr <- obj .: "ieeeAddr"
  manufacturerId <- obj .: "manufacturerID"
  manufacturerName <- obj .: "manufacturerName"
  model <- obj .: "model"
  networkAddress <- obj .: "networkAddress"
  powerSource <- obj .:? "powerSource"
  softwareBuildId <- obj .:? "softwareBuildId"
  stackVersion <- obj .:? "stackVersion"
  deviceType <- obj .: "type"
  zclVersion <- obj .:? "zclVersion"
  pure
    { applicationVersion
    , dateCode
    , friendlyName
    , hardwareVersion
    , ieeeAddr
    , manufacturerId
    , manufacturerName
    , model
    , networkAddress
    , powerSource
    , softwareBuildId
    , stackVersion
    , deviceType
    , zclVersion
    }

type Update =
  { installedVersion :: Int
  , latestVersion :: Int
  , state :: String
  }

decodeUpdate :: Json -> Either JsonDecodeError Update
decodeUpdate updateJson = do
  obj <- decodeJson updateJson
  installedVersion <- obj .: "installed_version"
  latestVersion <- obj .: "latest_version"
  state <- obj .: "state"
  pure { installedVersion, latestVersion, state }

--
-- I _think_ I want this to be a few different types, but not sure
-- what those are yet
--
type DeviceState =
  { device :: DeviceSummary

  , linkquality :: Int
  , update :: Maybe Update
  , updateAvailable :: Maybe Boolean

  -- light-related values
  , brightness :: Maybe Int
  , color :: Maybe Color
  , colorMode :: Maybe String -- probably should be enum
  , colorTemp :: Maybe Int
  , colorTempStartup :: Maybe Int
  , state :: Maybe String
  , gradient :: Maybe (Array String)
  , powerOnBehavior :: Maybe String -- should be enum?

  -- sensor-related values
  , contact :: Maybe Boolean
  , tamper :: Maybe Boolean
  , temperature :: Maybe Number
  , airQuality :: Maybe String -- should be enum?

  -- generally applicable values
  , voltage :: Maybe Int
  , battery :: Maybe Int
  , batteryLow :: Maybe Boolean
  }

decodeDeviceState :: Json -> Either JsonDecodeError DeviceState
decodeDeviceState deviceStateJson = do
  obj <- decodeJson deviceStateJson
  device <- decodeDeviceSummary =<< obj .: "device"
  linkquality <- obj .: "linkquality"
  update <- traverse decodeUpdate =<< obj .:? "update"
  updateAvailable <- obj .:? "update_available"

  -- from here below are optional
  brightness <- obj .:? "brightness"
  colorJson <- obj .:? "color" -- Either JsonDecodeError (Maybe Color)
  color <- for colorJson decodeColor
  colorMode <- obj .:? "color_mode"
  colorTemp <- obj .:? "color_temp"
  colorTempStartup <- obj .:? "color_temp_startup"
  state <- obj .:? "state"
  gradient <- obj .:? "gradient"
  powerOnBehavior <- obj .:? "power_on_behavior"
  contact <- obj .:? "contact"
  tamper <- obj .:? "tamper"
  temperature <- obj .:? "temperature"
  airQuality <- obj .:? "air_quality"
  voltage <- obj .:? "voltage"
  battery <- obj .:? "battery"
  batteryLow <- obj .:? "battery_low"
  pure
    { device
    , linkquality
    , update
    , updateAvailable
    , brightness
    , color
    , colorMode
    , colorTemp
    , colorTempStartup
    , state
    , gradient
    , powerOnBehavior
    , contact
    , tamper
    , temperature
    , airQuality
    , voltage
    , battery
    , batteryLow
    }
