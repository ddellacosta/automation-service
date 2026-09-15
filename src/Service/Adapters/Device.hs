{-# LANGUAGE TemplateHaskell #-}

module Service.Adapters.Device
  ( Device(..)
  , DeviceId
  , Devices(..)
  , name
  , ieeeAddress
  , manufacturer
  , modelId
  , capabilities
  )
  where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (FromJSON(..), (.:), (.:?), withArray, withObject)
import Data.Aeson.Types (parseFieldMaybe)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Vector (toList)
import Service.Adapters.Capability (Capabilities, parseCapabilities)

data Device = Device
  { _name :: T.Text
  , _ieeeAddress :: T.Text
  , _manufacturer :: Maybe T.Text
  , _modelId :: Maybe T.Text
  , _capabilities :: Capabilities
  } deriving (Eq, Show)

makeFieldsNoPrefix ''Device

--
-- Device.ieeeAddress
--
type DeviceId = T.Text

-- this is just a convenience type to allow me to easily call `decode`
-- on the message I get back from zigbee2mqtt/bridge/devices initially
data Devices = Devices
  { loadDevices :: [Device]
  } deriving (Eq, Show)

instance FromJSON Device where
  parseJSON = withObject "Device" $ \d -> do
    name' <- d .: "friendly_name"
    ieeeAddress' <- d .: "ieee_address"
    manufacturer' <- d .:? "manufacturer"
    modelId' <- d .:? "model_id"
    mExposes <-
      d .:? "definition" >>= maybe (pure Nothing) (flip parseFieldMaybe "exposes")
    capabilities' <- parseCapabilities mExposes
    pure $ Device name' ieeeAddress' manufacturer' modelId' capabilities'

instance FromJSON Devices where
 parseJSON = withArray "Devices" $ \a ->
    Devices <$> for (toList a) parseJSON
