module Test.Helpers
  ( loadTestDevices
  , 
  )
where

import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Service.Device (Device)
import Service.Messages.Zigbee2MQTT (parseDevices)

loadTestDevices :: IO [Device]
loadTestDevices = do
  devicesRawJSON <- BL.fromStrict <$> BS.readFile "test/fixtures/devices.json"
  pure $ fromMaybe [] $ parseDevices devicesRawJSON
