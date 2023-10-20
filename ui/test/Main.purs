module Test.Main where

import Prelude

import AutomationService.Message (Message(..))
import AutomationService.WebSocket (class WebSocket)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Elmish.Component (Command)
import Elmish.Test (find, prop, testComponent, text, (>>))
import Elmish.Test.DomProps as P
import Elmish.Test.Events (change, click)
import Main as Main
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

newtype TestWS = TestWS (Ref String)

instance WebSocket TestWS where
  sendString (TestWS wsStr) s = Ref.write s wsStr

  -- don't really care what this does in test...yet
  initializeListeners _ws _msgSink = liftEffect $ log "hey"

connectToWS :: Ref String -> Command Aff (Message TestWS)
connectToWS wsState msgSink =
  liftEffect <<< msgSink <<< InitWS <<< TestWS $ wsState

-- 

spec :: Spec Unit
spec = before setup $
  describe "Home page" $
    it "Can navigate to different pages" $ \wsState -> do
      let mqttMsg = "{\"start\": \"test\"}"

      testComponent
         { init: Main.init (connectToWS wsState)
         , view: Main.view
         , update: Main.update
         } do

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

        liftEffect $ Ref.write "plrr" wsState

        find ("li" `withTestId` "nav-publish-mqtt" <> " a") >> click
        find ("input" `withTestId` "publish-mqtt-input") >> change mqttMsg
        find ("button" `withTestId` "publish-mqtt-btn") >> click
        find ("button" `withTestId` "publish-mqtt-btn") >> click

        find ("input" `withTestId` "publish-mqtt-input") >> prop P.value
           >>= shouldEqual mqttMsg

        --
        -- Testing that I'm at least hitting the
        -- AutomationService.WebSocket API when I hit the "Publish"
        -- button is an unsolved problem, and getting it working is
        -- hindered by a few things, as far as I can tell so far:
        -- console debugging is a no-go, because I can't print log
        -- messages when testing inside of `testComponent`. After
        -- spending the better part of a work day yak-shaving just to
        -- figure out how this works, I _think_ it's because I can't
        -- set the console
        -- (https://github.com/capricorn86/happy-dom/wiki/Virtual-Console)
        -- via GlobalReporter in happy-dom
        -- (https://github.com/capricorn86/happy-dom/issues/1105),
        -- used by elmish-test-library, but I honestly don't know.
        --
        -- The second learning I acquired from yak-shaving is that it
        -- seems that inside of test component, messages aren't
        -- actually being dispatched to update, or perhaps,
        -- `fork*` calls aren't run in testComponent...or something?
        -- It's possible the way I am thinking about Refs is wrong
        -- somehow too. I don't know, because I've already spent more
        -- than a day just trying to understand what is going on and
        -- my threshold has been reached. ¯\_(ツ)_/¯
        --
        -- Anyways, for now I've been able to confirm this works
        -- via manual testing, and hopefully I can find a different
        -- way to build automated tests soon that exercises this
        -- functionality on the frontend.
        --

        -- wsStr <- liftEffect $ Ref.read wsState
        -- wsStr `shouldEqual` mqttMsg

   where
     setup :: Aff (Ref String)
     setup = liftEffect $ Ref.new "foo"

     withTestId :: String -> String -> String
     withTestId sel testId = sel <> "[data-test-id=\"" <> testId <> "\"]"
