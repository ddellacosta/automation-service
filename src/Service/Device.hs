{-# LANGUAGE TemplateHaskell #-}

module Service.Device
  ( DeviceId(..)
  , Device(..)
  , findDevice
  , id
  , name
  , nullDevice
  , parseDeviceId
  , topic
  )
where

import Prelude hiding (id)

import Control.Lens ((^?), folded, filtered, makeFieldsNoPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.MQTT.Client (Topic)

data DeviceId = NullDevice | TestDevice | GledoptoGLC007P_1
  deriving (Generic, Show, Eq, Ord)

-- I would like this to be somehow global, so instances can be added from other than this location
parseDeviceId :: Text -> DeviceId
parseDeviceId = \case
  "GledoptoGLC007P_1" -> GledoptoGLC007P_1
  _ -> NullDevice

data Device = Device
  { _id :: DeviceId
  , _name :: Text
  , _topic :: Topic
  }
  deriving (Generic, Show, Eq)

makeFieldsNoPrefix ''Device

-- |
-- | A Device with the DeviceId of `Null`, name "Null" and topic "null". Placeholder for when we
-- | are expecting a Device but don't have one, for whatever reason.
-- |
nullDevice :: Device
nullDevice = Device NullDevice "Null" "null"

-- |
-- | findDevice returns either the device matching the DeviceId, or
-- | nullDevice if nothing is found.
-- |
findDevice :: DeviceId -> [Device] -> Device
findDevice deviceId devices =
  fromMaybe nullDevice $ devices ^? folded . filtered ((== deviceId) . _id)
