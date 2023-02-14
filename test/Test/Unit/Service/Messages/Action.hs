module Test.Unit.Service.Messages.Action
  ( spec
  ,
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson (decode, object)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Messages.Action (Action(..))
import Service.ActionName (ActionName(Gold))


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
