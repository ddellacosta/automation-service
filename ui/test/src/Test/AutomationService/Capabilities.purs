module Test.AutomationService.Capabilities where

import AutomationService.Capabilities (Capability(..), FeatureType(..), SubProps(..), canGet, canSet,
                                       capabilityDetails, decodeCapabilities, isPublished)
import AutomationService.Capabilities.Optics (_access, _description, _featureType, _label, _name, _property, _subProps)
import Data.Argonaut.Decode ((.:), (.:?), fromJsonString)
import Data.Array.NonEmpty (fromArray)
import Data.Lens ((^?), _Just, _Right, folded, lengthOf, to)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, ($), (<<<), (<$>), (=<<), bind, discard)
import Test.AutomationService.Spec (Spec)
import Test.AutomationService.Helpers (shouldHaveCapabilities)
import Test.Fixtures (signeFixture)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "decoding Capabilities" $
    it "Can parse a collection of exposes JSON" $ do

      let
        signe = do
          obj <- fromJsonString signeFixture
          definition <- obj .: "definition"
          exposes <- definition .:? "exposes"
          decodeCapabilities $ unsafePartial $ fromJust $ fromArray =<< exposes

        brightness = signe ^? _Right <<< ix 1 <<< to capabilityDetails
        prop cap przm = cap ^? (_Just <<< przm)
        mProp cap przm = cap ^? (_Just <<< przm <<< _Just)

        -- I can't seem to compose these with prop/mProp without a
        -- type error about not matching String, but it highlights the
        -- `ix` call for some reason?
        featureType = signe ^? _Right <<< ix 1 <<< to capabilityDetails <<< _featureType <<< _Just
        access = signe ^? _Right <<< ix 1 <<< to capabilityDetails <<< _access
        subProps = signe ^? _Right <<< ix 1 <<< to capabilityDetails <<< _subProps

        subPropsFixture
          = Numeric
            { unit: Nothing
            , valueMax: (Just 254)
            , valueMin: (Just 0)
            , valueStep: Nothing
            }


      lengthOf (folded <<< folded) signe `shouldEqual` 11

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

      signe `shouldHaveCapabilities`
        [ Brightness
        , ColorGradient
        , ColorHue
        , ColorTempStartup
        , ColorTemperature
        , ColorXY
        , Effect
        , GradientScene
        , LinkQuality
        , OnOff
        , PowerOnBehavior
        ]
