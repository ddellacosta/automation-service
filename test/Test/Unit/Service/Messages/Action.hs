module Test.Unit.Service.Messages.Action
  ( spec_
  ,
  )
where

import Data.Aeson (decode)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Service.Messages.Action (Action(..))
import Service.ActionName (ActionName(Gold))


spec_ :: Spec
spec_ = describe "Action message parsing" $ do
  it "correctly parses well-formed Action messages" $ do
    (decode "{\"start\": \"Gold\"}" :: Maybe (Action Text))
      `shouldBe`
      Just (Start Gold)

    (decode "{\"stop\": \"Gold\"}" :: Maybe (Action Text))
      `shouldBe`
      Just (Stop Gold)

    (decode "{\"send\": \"Gold\", \"msg\": \"hey\"}" :: Maybe (Action Text))
      `shouldBe`
      Just (SendTo Gold "hey")

  it "returns a Null Action message when given an unparseable message" $ do
    (decode "{\"what\": \"nope\"}" :: Maybe (Action Text))
      `shouldBe`
      Just Null
