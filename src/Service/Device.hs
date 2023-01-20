{-# LANGUAGE TemplateHaskell #-}

module Service.Device where

import Control.Lens ((^?), folded, filtered, makeFieldsNoPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.MQTT.Client (Topic)

data DeviceId = Null | GledoptoGLC007P_1
  deriving (Generic, Show, Eq, Ord)

-- I would like this to be somehow global, so instances can be added from other than this location
parseDeviceId :: Text -> DeviceId
parseDeviceId = \case
  "GledoptoGLC007P_1" -> GledoptoGLC007P_1
  _ -> Null

data Device = Device
  { _id :: DeviceId
  , _name :: Text
  , _topic :: Topic
  }
  deriving (Generic, Show)

makeFieldsNoPrefix ''Device

nullDevice :: Device
nullDevice = Device Null "Null" "null"

findDevice :: DeviceId -> [Device] -> Device
findDevice deviceId devices =
  fromMaybe nullDevice $ devices ^? folded . filtered ((== deviceId) . _id)
