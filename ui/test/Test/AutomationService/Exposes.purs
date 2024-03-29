module Test.AutomationService.Exposes where

import AutomationService.Exposes (CapType(..), FeatureType(..), SubProps(..),
                                  _access, _description, _featureType, _label, _name,
                                  _property, _subProps, _type, canGet, canSet,
                                  isPublished, decodeExposes)
import Data.Argonaut.Decode ((.:), (.:?), fromJsonString)
import Data.Lens ((^?), _Just, _Right, folded, lengthOf)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (Unit, ($), (<<<), (<$>), bind, discard)
import Test.Fixtures (signeFixture)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec =
  describe "Exposes" $
    it "Can parse a collection of Exposes JSON" $ do

      let
        signeExposes = do
          obj <- fromJsonString signeFixture
          definition <- obj .: "definition"
          exposes' <- definition .:? "exposes"
          decodeExposes Nothing (fromMaybe [] exposes')

        brightness = signeExposes ^? _Right <<< ix 1
        prop cap przm = cap ^? (_Just <<< przm)
        mProp cap przm = cap ^? (_Just <<< przm <<< _Just)

        -- I can't seem to compose these with prop/mProp without a
        -- type error about not matching String, but it highlights the
        -- `ix` call for some reason?
        featureType = signeExposes ^? _Right <<< ix 1 <<< _featureType <<< _Just
        type' = signeExposes ^? _Right <<< ix 1 <<< _type
        access = signeExposes ^? _Right <<< ix 1 <<< _access
        subProps = signeExposes ^? _Right <<< ix 1 <<< _subProps

        subPropsFixture
          = Numeric
            { unit: Nothing
            , valueMax: (Just 254)
            , valueMin: (Just 0)
            , valueStep: Nothing
            }

      -- assertions

      lengthOf (_Right <<< folded) signeExposes `shouldEqual` 11

      type' `shouldEqual` Just Numeric'

      featureType `shouldEqual` Just Light

      brightness `prop` _name
        `shouldEqual` Just "brightness"

      brightness `mProp` _description
        `shouldEqual` Just "Brightness of this light"

      brightness `prop` _label
        `shouldEqual` Just "Brightness"

      brightness `mProp` _property
        `shouldEqual` Just "brightness"

      (canGet <$> access) `shouldEqual` Just true
      (canSet <$> access) `shouldEqual` Just true
      (isPublished <$> access) `shouldEqual` Just true

      subProps `shouldEqual` Just subPropsFixture
