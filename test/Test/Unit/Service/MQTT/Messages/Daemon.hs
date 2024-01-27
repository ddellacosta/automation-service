{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Unit.Service.MQTT.Messages.Daemon
  ( spec
  ,
  )
where

import Control.Lens (_1, _2, _Just, (^?))
import Data.Aeson (decode, object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Network.MQTT.Topic (mkTopic)
import Service.Automation (ClientMsg (..), _ValueMsg)
import Service.AutomationName (AutomationName (Gold, HTTP, LuaScript), Port (..))
import Service.MQTT.Messages.Daemon (Message (..), _SendTo)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = describe "Automation message parsing" $ do
  it "correctly parses well-formed Automation messages" $ do
    (decode "{\"start\": \"Gold\"}" :: Maybe Message)
      `shouldBe`
      Just (Start Gold)

    (decode "{\"stop\": \"Gold\"}" :: Maybe Message)
      `shouldBe`
      Just (Stop Gold)

    -- SendTo
    let
      sendToGold :: Maybe Message
      sendToGold =
        decode "{\"send\": \"Gold\", \"msg\": {\"mood\": \"frumpy\", \"fancy\": true}}"

    sendToGold ^? _Just . _SendTo . _1
      `shouldBe`
      Just Gold

    sendToGold ^? _Just . _SendTo . _2 . _ValueMsg . key "mood"
      `shouldBe`
      Just (Aeson.String "frumpy")

    sendToGold ^? _Just . _SendTo . _2 . _ValueMsg . key "fancy"
      `shouldBe`
      Just (Aeson.Bool True)

    -- Kinda redundant but good to see it all laid out like this,
    -- whereas the above are helpful for understanding how to access
    -- values inside.
    sendToGold `shouldBe`
      Just
      ( SendTo Gold
        ( ValueMsg
          ( object
            [ ("fancy", Aeson.Bool True)
            , ("mood", Aeson.String "frumpy")
            ]
          )
        )
      )

    -- Schedule
    let
      (Just (Schedule jobId sched msg)) =
        decode "{\"job\": {\"start\": \"Gold\"}, \"jobId\": \"myJob\", \"schedule\": \"* * * * *\"}"

    jobId `shouldBe` "myJob"
    sched `shouldBe` "* * * * *"
    msg `shouldBe` Start Gold

   -- Unschedule
    let
      (Just (Unschedule jobId')) =
        decode "{\"unschedule\": \"myJob\"}"

    jobId' `shouldBe` "myJob"

    -- Subscribe
    decode "{\"subscribe\": \"HTTP 8080\", \"topic\": \"myTopic\"}"
      `shouldBe`
      Just (Subscribe (HTTP (Port 8080)) (mkTopic "myTopic"))

  it "returns a Null Automation message when given an unparseable message" $ do
    (decode "{\"what\": \"nope\"}" :: Maybe Message)
      `shouldBe`
      Just Null

  it "converts lower-case start terms to LuaScript AutomationNames" $ do
    (decode "{\"start\": \"fooBar\"}" :: Maybe Message)
      `shouldBe`
      Just (Start (LuaScript "fooBar"))
