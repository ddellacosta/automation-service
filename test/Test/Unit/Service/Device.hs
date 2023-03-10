module Test.Unit.Service.Device
  ( spec
  ,
  )
where

import Prelude hiding (lookup)

import Control.Lens ((^?))
import Data.Aeson.Lens (_String, key)
import Data.Maybe (fromJust)
import Network.MQTT.Topic (Topic(..))
import Service.Device (Device(..), parseTopic, toLuaDevice)
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
