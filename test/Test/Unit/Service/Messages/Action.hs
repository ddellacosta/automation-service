module Test.Unit.Service.Messages.Action
  ( spec
  ,
  )
where

import Control.Lens ((^?), _1, _2, _Just)
import qualified Data.Aeson as Aeson
import Data.Aeson (decode, object)
import Data.Aeson.Lens (key)
import Service.ActionName (ActionName(Gold))
import Service.Messages.Action (Action(..), _SendTo)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = describe "Action message parsing" $ do
  it "correctly parses well-formed Action messages" $ do
    (decode "{\"start\": \"Gold\"}" :: Maybe Action)
      `shouldBe`
      Just (Start Gold)

    (decode "{\"stop\": \"Gold\"}" :: Maybe Action)
      `shouldBe`
      Just (Stop Gold)

    let
      sendToGold :: Maybe Action
      sendToGold =
        decode "{\"send\": \"Gold\", \"msg\": {\"mood\": \"frumpy\", \"fancy\": true}}"

    sendToGold ^? _Just . _SendTo . _1
      `shouldBe`
      Just Gold

    sendToGold ^? _Just . _SendTo . _2 . key "mood"
      `shouldBe`
      Just (Aeson.String "frumpy")

    sendToGold ^? _Just . _SendTo . _2 . key "fancy"
      `shouldBe`
      Just (Aeson.Bool True)

    -- Kinda redundant but good to see it all laid out like this,
    -- whereas the above are helpful for understanding how to access
    -- values inside.
    sendToGold `shouldBe`
      Just
      ( SendTo Gold
        ( object
          [ ("fancy", Aeson.Bool True)
          , ("mood", Aeson.String "frumpy")
          ]
        )
      )

  it "returns a Null Action message when given an unparseable message" $ do
    (decode "{\"what\": \"nope\"}" :: Maybe Action)
      `shouldBe`
      Just Null
