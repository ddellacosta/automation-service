module Test.Unit.Service.Device
  ( spec
  ,
  )
where

import Prelude hiding (id, lookup)

import Control.Lens ((^.), (^?))
import Data.Aeson.Lens (_String, key)
import Data.Maybe (fromJust)
import Network.MQTT.Topic (Topic (..))
import Service.Device (Device (..), category, id, manufacturer, model, name, toLuaDevice, topic,
                       topicGet, topicSet)
import Service.MQTT.Topic (parseTopic)
import Test.Helpers (loadTestDevices)
import Test.Hspec (Spec, describe, it, shouldBe)

deviceFix :: Device
deviceFix = Device
  { _id = "1234"
  , _name = "MyDevice"
  , _category = "EndDevice"
  , _manufacturer = Just "HACo"
  , _model = Just "SuperSensor"
  , _topic = parseTopic "zigbee2mqtt/MyDevice"
  , _topicGet = parseTopic "zigbee2mqtt/MyDevice/get"
  , _topicSet = parseTopic "zigbee2mqtt/MyDevice/set"
  }

spec :: Spec
spec = describe "Devices" $ do
  it "Generates JSON Value-typed Devices for passing to Lua" $ do
    let
      luaDevice = fromJust . toLuaDevice $ deviceFix
      lookup = fromJust . (\k -> luaDevice ^? key k . _String)

    lookup "id" `shouldBe` _id deviceFix
    lookup "name" `shouldBe` _name deviceFix
    lookup "category" `shouldBe` _category deviceFix
    lookup "model" `shouldBe` (fromJust . _model $ deviceFix)
    lookup "manufacturer" `shouldBe` (fromJust . _manufacturer $ deviceFix)
    lookup "topic" `shouldBe` unTopic (_topic deviceFix)
    lookup "topicGet" `shouldBe` unTopic (_topicGet deviceFix)
    lookup "topicSet" `shouldBe` unTopic (_topicSet deviceFix)

  it "parses devices from the output of zigbee2mqtt/bridge/devices" $ do
    -- Is it okay that the code-under-test is inside the test helper?
    -- 1) DRYer which is whatever, 2) couples this test up with other
    -- uses of loading and parsing device test fixtures, which is
    -- arguably good since that shouldn't be changing from use-case to
    -- use-case, and if changing the helper code triggers failures it
    -- can point to larger system-wide breakage, potentially.
    devices <- loadTestDevices

    let mirrorLight = devices !! 2

    (mirrorLight ^. id) `shouldBe` "0xb4e3f9fffe14c707"
    (mirrorLight ^. name) `shouldBe` "Mirror Light Strip"
    (mirrorLight ^. category) `shouldBe` "Router"
    (mirrorLight ^. manufacturer) `shouldBe` Just "GLEDOPTO"
    (mirrorLight ^. model) `shouldBe` Just "GL-C-007P"
    (mirrorLight ^. topic) `shouldBe` "zigbee2mqtt/Mirror Light Strip"
    (mirrorLight ^. topicGet) `shouldBe` "zigbee2mqtt/Mirror Light Strip/get"
    (mirrorLight ^. topicSet) `shouldBe` "zigbee2mqtt/Mirror Light Strip/set"

    (3 :: Int) `shouldBe` (2 :: Int)
