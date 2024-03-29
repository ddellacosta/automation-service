module Test.AutomationService.Device where

import AutomationService.Device (decodeDevice, _deviceDetails, _category, _id,
                                 _exposes, _manufacturer, _model, _name)
import Data.Argonaut.Decode (parseJson)
import Data.Lens (_Just, _Right, folded, lengthOf, preview)
import Data.Maybe (Maybe(..))
import Prelude (Unit, ($), (<<<), (=<<), discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Fixtures (signeFixture)

spec :: Spec Unit
spec =
  describe "decoding a Device" $
    it "Decode all properties of a Device" $ do

      let
        -- code under test
        signe = decodeDevice =<< parseJson signeFixture

        -- optics are so useful as test helpers
        signe' lns = preview (_Right <<< _deviceDetails <<< lns) signe
        signeM lns = preview (_Right <<< _deviceDetails <<< lns <<< _Just) signe
        _exposes' = _Right <<< _deviceDetails <<< _exposes <<< folded

      signe' _id  `shouldEqual` Just "0x001788010c52373e"
      signe' _name  `shouldEqual` Just "Basement Black Signe"
      signe' _category `shouldEqual` Just "Router"
      signeM _manufacturer `shouldEqual` Just "Philips"
      signeM _model `shouldEqual` Just "915005987601"
      lengthOf _exposes' signe `shouldEqual` 11
