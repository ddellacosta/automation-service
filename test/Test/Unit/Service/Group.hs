module Test.Unit.Service.Group
  ( spec
  ,
  )
where

import Prelude hiding (id, lookup)

import Control.Lens ((^.), (^?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (_Array, _String, key)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Network.MQTT.Topic (Topic(..))
import Service.Group
  ( Group(..)
  , Member(..)
  , Scene(..)
  , id
  , members
  , name
  , scenes
  , toLuaGroup
  , topic
  , topicGet
  , topicSet
  )
import Service.MQTT.Topic (parseTopic)
import Test.Helpers (loadTestGroups)
import Test.Hspec (Spec, describe, it, shouldBe)

groupFix :: Group
groupFix = Group
  { _id = 1234
  , _name = "MyGroup"
  , _members = [ Member "m1" 11, Member "m2" 11 ]
  , _scenes = [ Scene 1 "foo", Scene 2 "bar" ]
  , _topic = parseTopic "zigbee2mqtt/MyGroup"
  , _topicGet = parseTopic "zigbee2mqtt/MyGroup/get"
  , _topicSet = parseTopic "zigbee2mqtt/MyGroup/set"
  }

spec :: Spec
spec = describe "Groups" $ do
  it "Generates JSON Value-typed Groups for passing to Lua" $ do
    let
      luaGroup = fromJust . toLuaGroup $ groupFix
      lookup = fromJust . (\k -> luaGroup ^? key k . _String)
      lookupArray = fromJust . (\k -> luaGroup ^? key k . _Array)

    (luaGroup ^? key "id") `shouldBe` Just (Aeson.Number 1234)
    lookup "name" `shouldBe` _name groupFix
    lookupArray "members" `shouldBe`
      V.fromList
        [ Aeson.object [("memberId", "m1"), ("endpoint", Aeson.Number 11)]
        , Aeson.object [("memberId", "m2"), ("endpoint", Aeson.Number 11)]
        ]
    lookupArray "scenes" `shouldBe`
      V.fromList
        [ Aeson.object [("sceneId", Aeson.Number 1), ("sceneName", "foo")]
        , Aeson.object [("sceneId", Aeson.Number 2), ("sceneName", "bar")]
        ]
    lookup "topic" `shouldBe` unTopic (_topic groupFix)
    lookup "topicGet" `shouldBe` unTopic (_topicGet groupFix)
    lookup "topicSet" `shouldBe` unTopic (_topicSet groupFix)

  it "Parses groups from the output of zigbee2mqtt/bridge/groups" $ do
    -- see comment in groups about code-under-test
    groups <- loadTestGroups

    let basementStandingLamp = head groups

    basementStandingLamp ^. id `shouldBe` 1
    basementStandingLamp ^. name `shouldBe` "Basement Standing Lamp"
    basementStandingLamp ^. members `shouldBe`
      [ Member "0x001788010c9d3dc7" 11
      , Member "0x001788010c48279d" 11
      ]
    basementStandingLamp ^. scenes `shouldBe` [Scene 0 "Chrimbus", Scene 1 "Chilly"]
    basementStandingLamp ^. topic `shouldBe` "zigbee2mqtt/Basement Standing Lamp"
    basementStandingLamp ^. topicGet `shouldBe` "zigbee2mqtt/Basement Standing Lamp/get"
    basementStandingLamp ^. topicSet `shouldBe` "zigbee2mqtt/Basement Standing Lamp/set"
