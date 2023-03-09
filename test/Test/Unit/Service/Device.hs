module Test.Unit.Service.Device
  ( spec
  ,
  )
where

import Control.Lens ((^?), ix)
import Data.Aeson (Value, decode, encode)
import Data.Aeson.Lens (_String, key)
import Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import Network.MQTT.Topic (Topic(..))
import Service.Device (Device(..), parseTopic, toLuaDevice)
import Test.Hspec (Spec, describe, it, shouldBe)

deviceFix = Device
  { _id = "1234"
  , _name = "MyDevice"
  , _category = "EndDevice"
  , _manufacturer = Just "HACo"
  , _model = Just "SuperSensor"
  , _getTopic = parseTopic "zigbee2mqtt/MyDevice/get"
  , _setTopic = parseTopic "zigbee2mqtt/MyDevice/set"
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
    lookup "getTopic" `shouldBe` unTopic (_getTopic deviceFix)
    lookup "setTopic" `shouldBe` unTopic (_setTopic deviceFix)
