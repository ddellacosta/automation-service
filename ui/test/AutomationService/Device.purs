module Test.AutomationService.Device where

import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Device" $
    it "Can do the thing" $ \wsState -> do

      "Should" `shouldEqual` "hey"
