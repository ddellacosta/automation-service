module Test.Main where

import Prelude

import AutomationService.Message (Message(..))
import AutomationService.WebSocket (class WebSocket)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (debug)
import Elmish (Transition)
import Elmish.Test (find, testComponent, text, (>>))
import Elmish.Test.Events (click)
import Main as Main
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

-- WebSocket stub

newtype TestWS = TestWS String

instance WebSocket TestWS where
  connectToWS msgSink = liftEffect $ msgSink (InitWS (TestWS "Fake"))
  initializeListeners _ws _msgSink = liftEffect $ debug "hey"

type TestInit = Transition (Message TestWS) (Main.State TestWS)

-- 

spec :: Spec Unit
spec =
  describe "Home page" $
    it "Can navigate to different pages" $
      testComponent { init: (Main.init :: TestInit), view: Main.view, update: Main.update } do
        find ("h2" `withTestId` "main-title") >> text
          >>= shouldEqual "Home"

        -- Devices
        find ("li" `withTestId` "nav-devices" <> " a") >> click
        find ("h2" `withTestId` "main-title") >> text
          >>= shouldEqual "Devices"

        -- Publish MQTT
        find ("li" `withTestId` "nav-publish-mqtt" <> " a") >> click
        find ("h2" `withTestId` "main-title") >> text
          >>= shouldEqual "Publish MQTT"

        find ("li" `withTestId` "nav-publish-mqtt" <> " a") >> click
        find ("button" `withTestId` "publish-mqtt-btn") >> click
        -- WIP

  where
    withTestId :: String -> String -> String
    withTestId sel testId = sel <> "[data-test-id=\"" <> testId <> "\"]"
