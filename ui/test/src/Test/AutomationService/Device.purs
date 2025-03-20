module Test.AutomationService.Device where

import AutomationService.Device (Device(..), decodeDevice, _deviceDetails, _category, _id,
                                 _exposes, _manufacturer, _model, _name)
import Data.Argonaut.Decode (parseJson)
import Data.Lens (_Just, _Right, folded, lengthOf, preview)
import Data.Maybe (Maybe(..))
import Prelude (Unit, ($), (<<<), (=<<), discard)
import Test.AutomationService.Helpers (shouldConstruct)
import Test.AutomationService.Spec (Spec)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Fixtures (airQualitySensorFixture, contactSensorFixture, controlsDeviceFixture, humiditySensorFixture, lightSensorFixture, signeFixture, unknownDeviceFixture, windowCoveringFixture)

spec :: Spec Unit
spec =
  describe "decoding a Device" $
    it "Decode all properties of a Device" $ do

      let
        -- code under test (decodeDevice)
        signe = decodeDevice =<< parseJson signeFixture
        controlsDevice = decodeDevice =<< parseJson controlsDeviceFixture
        contactSensor = decodeDevice =<< parseJson contactSensorFixture
        lightSensor = decodeDevice =<< parseJson lightSensorFixture
        airQualitySensor = decodeDevice =<< parseJson airQualitySensorFixture
        humiditySensor = decodeDevice =<< parseJson humiditySensorFixture
        windowCovering = decodeDevice =<< parseJson windowCoveringFixture
        unknownDevice = decodeDevice =<< parseJson unknownDeviceFixture

        signe' lns = preview (_Right <<< _deviceDetails <<< lns) signe
        signeM lns = preview (_Right <<< _deviceDetails <<< lns <<< _Just) signe
        _exposes' = _Right <<< _deviceDetails <<< _exposes <<< folded

      signe' _id  `shouldEqual` Just "0x001788010c52373e"
      signe' _name  `shouldEqual` Just "Basement Black Signe"
      signe' _category `shouldEqual` Just "Router"
      signeM _manufacturer `shouldEqual` Just "Philips"
      signeM _model `shouldEqual` Just "915005987601"
      lengthOf _exposes' signe `shouldEqual` 11

      --
      -- Implemented but untested I guess because I didn't base these
      -- on a real device?
      --
      -- ColorTemperatureLight'
      -- DimmableLight'
      -- OnOffLight'
      -- TemperatureSensor'
      --
      -- Also, all my OccupancySensors are LightSensors, it turns out ¯\_(ツ)_/¯
      --
      ExtendedColorLight `shouldConstruct` signe
      GenericSwitch `shouldConstruct` controlsDevice
      ContactSensor `shouldConstruct` contactSensor
      LightSensor `shouldConstruct` lightSensor
      AirQualitySensor `shouldConstruct` airQualitySensor
      HumiditySensor `shouldConstruct` humiditySensor
      WindowCovering `shouldConstruct` windowCovering
      UnknownDevice `shouldConstruct` unknownDevice
