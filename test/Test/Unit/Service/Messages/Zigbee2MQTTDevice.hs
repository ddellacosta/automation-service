module Test.Unit.Service.Messages.Zigbee2MQTTDevice
  ( spec
  ,
  )
where

import Prelude hiding (id)

import Control.Lens ((^.))
import Network.MQTT.Topic (mkTopic)
import Service.Device (id, category, manufacturer, model, name)
import Service.Messages.Zigbee2MQTTDevice (deviceSetterTopic)
import Test.Helpers (loadTestDevices)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Zigbee2MQTT device info message parsing" $ do
  it "parses devices from the output of zigbee2mqtt/bridge/devices" $ do
    -- Is it okay that the code-under-test is inside the test helper?
    -- 1) DRYer which is whatever, 2) couples this test up with other
    -- uses of loading and parsing device test fixtures, which is
    -- arguably good since that shouldn't be changing from use-case to
    -- use-case, and if changing the helper code triggers failures it
    -- can point to larger system-wide breakage, potentially.
    devices <- loadTestDevices

    let
      mirrorLight = devices !! 2
      topic = mkTopic "zigbee2mqtt/Mirror Light Strip/set"

    (mirrorLight ^. id) `shouldBe` "0xb4e3f9fffe14c707"
    (mirrorLight ^. name) `shouldBe` "Mirror Light Strip"
    (mirrorLight ^. category) `shouldBe` "Router"
    (mirrorLight ^. manufacturer) `shouldBe` Just "GLEDOPTO"
    (mirrorLight ^. model) `shouldBe` Just "GL-C-007P"

    deviceSetterTopic mirrorLight `shouldBe` topic
