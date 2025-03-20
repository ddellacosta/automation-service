module Test.AutomationService.Group where

import AutomationService.Group (decodeGroups)
import AutomationService.Device (Decoded(..), DecodedStatus(..), Device(..), decodeDevices, details,
                                 mkFailedParse)
import Data.Array (head)
import Data.Array as Array
import Data.Argonaut.Decode (parseJson)
import Data.Either (fromRight)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Prelude (Unit, (<<<), ($), (<$>), (=<<), discard)
import Test.AutomationService.Helpers (shouldConstruct)
import Test.AutomationService.Spec (Spec)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Fixtures (livingRoomStandingLampBulbsFixture, livingRoomStandingLampGroupFixture)

spec :: Spec Unit
spec =
  describe "decoding Groups" $
    it "Decode all properties of a Group" $ do

      let
        parsedJson = parseJson livingRoomStandingLampBulbsFixture

        bulbs = case fromRight (mkFailedParse parsedJson) $ decodeDevices <$> parsedJson of
          Decoded DecodingSucceeded { devices } -> devices
          _ -> M.empty

        -- code under test (decodeGroups)
        lampGroup = head <<< fromRight [] $
          decodeGroups bulbs =<< parseJson livingRoomStandingLampGroupFixture

        lampDevices = _.members <$> lampGroup

        -- helpers to reduce a little boilerplate
        deviceAt idx ds = _.device <$> Array.index ds idx
        deviceDetailAt idx ds detail = detail <<< details <$> deviceAt idx ds

      for_ lampDevices \lampDevices' -> do
        ExtendedColorLight `shouldConstruct` deviceAt 0 lampDevices'
        ExtendedColorLight `shouldConstruct` deviceAt 1 lampDevices'
        ExtendedColorLight `shouldConstruct` deviceAt 2 lampDevices'

        -- should be deterministic based on the order of items in the JSON
        Just "Living Room Standing 40W 1"
          `shouldEqual`
          deviceDetailAt 0 lampDevices' _.name

        Just "Living Room Standing 40W 2"
          `shouldEqual`
          deviceDetailAt 1 lampDevices' _.name

        Just "Living Room Standing 40W 3"
          `shouldEqual`
          deviceDetailAt 2 lampDevices' _.name
