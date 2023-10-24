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
        -- Kind of trivial, but helps validate that the message
        -- was sent and some actions were triggered when the
        -- 'Publish' button is clicked, at least.
        --
        find ("div" `withTestId` "last-sent-msg") >> text
          >>= shouldEqual ("Last sent:\n" <> mqttMsg)

        --
        -- I wanted to test that I was actually hitting the API using
        -- a Ref, but for reasons I don't understand sendString
        -- doesn't get triggered inside this test. Maybe it has to do
        -- with how `fork` and its siblings work in the context of
        -- textComponent? Needs more digging. On a side note, I
        -- couldn't figure out why I can't log inside of
        -- testComponent, but I think it has to do with the virtual
        -- console in Happy Dom...it makes it harder to debug, for
        -- sure:
        -- (https://github.com/capricorn86/happy-dom/wiki/Virtual-Console)
        -- (This may prevent this from getting fixed inside of
        -- elmish-test-library, for now?)
        -- (https://github.com/capricorn86/happy-dom/issues/1105)
        --
        -- wsStr <- liftEffect $ Ref.read wsState
        -- wsStr `shouldEqual` mqttMsg

   where
     setup :: Aff (Ref String)
     setup = liftEffect $ Ref.new "foo"

     withTestId :: String -> String -> String
     withTestId sel testId = sel <> "[data-test-id=\"" <> testId <> "\"]"
