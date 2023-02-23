module Test.Unit.Service.Messages.Zigbee2MQTTDevice
  ( spec
  ,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.MQTT.Topic (mkTopic)
import Service.Messages.Zigbee2MQTTDevice (deviceSetterTopic, parseDevices)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = describe "Zigbee2MQTT device info message parsing" $ do
  it "parses devices from the output of zigbee2mqtt/bridge/devices" $ do
    devicesRawJSON <- BL.fromStrict <$> BS.readFile "test/fixtures/devices.json"
    let
      devices = parseDevices devicesRawJSON
      gledoptoGLC007P = (!! 2) <$> devices
      topic = mkTopic "zigbee2mqtt/Gledopto GL-C-007P RGBW LED Controller Pro/set"

    (gledoptoGLC007P >>= deviceSetterTopic)
      `shouldBe`
      topic
