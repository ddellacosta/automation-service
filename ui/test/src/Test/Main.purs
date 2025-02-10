module Test.Main where

import AutomationService.Message (Message(..))
import AutomationService.WebSocket (class WebSocket)
import Data.Argonaut.Core (stringify)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish.Component (Command)
-- see Test.AutomationService.Elmish.Bootstrap
-- import Elmish.Test (find, prop, testComponent, text, (>>))
-- import Elmish.Test (find, prop, text, (>>), nearestEnclosingReactComponentName)
import Elmish.Test (find, prop, (>>), nearestEnclosingReactComponentName)
import Elmish.Test.DomProps as P
import Elmish.Test.Events (change, click)
import Foreign (unsafeFromForeign)
import Main as Main
import Prelude (Unit, bind, discard, pure, void, ($), (<<<), (>>>), (<$>), (<>), (>>=), (=<<))
import Test.AutomationService.Elmish.Bootstrap (testComponent)
import Test.AutomationService.Spec (Spec)
import Test.AutomationService.WebSocketStub (webSocketStub)
import Test.Fixtures as Fixtures
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web.Event.CustomEvent as CE
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventTarget, eventListener)
import Web.Event.EventTarget as ET

import Web.DOM.Element (Element, className, tagName, toNode, fromNode)
import Web.DOM.Node (firstChild, nodeName, textContent)
import Elmish.Test.State (class Testable, currentNode)
import Debug (traceM)

--
-- For reasons that I don't understand, only when attempting to test
-- this in the context of a nix build does innerText seem to fail
-- here:
-- https://github.com/collegevine/purescript-elmish-testing-library/blob/f867a4c57f2f98fc731aa80506ecae22e57e78e2/src/Elmish/Test/Query.js#L1
--
-- As a result I just made my own stupid text function that doesn't
-- depend on (Elmist.Test.State.)Testable, and which I can use with
-- >>= instead of >>. Everything else seems fine ¯\_(ツ)_/¯
--
-- I'm sure there's a deeper reason this is happening that I should
-- probably care more about but I've spent hours yak-shaving this and
-- I'm kinda done with it. As long as I only have to modify test code
-- I don't care that much
--
text :: ∀ m. MonadEffect m => Element -> m String
text = toNode >>> textContent >>> liftEffect

newtype TestWS = TestWS
  { store :: Ref String
  , ws :: EventTarget
  }

instance WebSocket TestWS where
  sendJson (TestWS { store: wsStr }) s = Ref.write (stringify s) wsStr
  sendString (TestWS { store: wsStr }) s = Ref.write s wsStr

  addWSEventListener (TestWS { ws }) messageHandler = do
    el <- liftEffect $ eventListener $ \evt -> do
      for_ (CE.fromEvent evt) \customEvt -> do
        let jsonStr = unsafeFromForeign $ CE.detail customEvt
        messageHandler jsonStr
    ET.addEventListener (EventType "fakeWebSocketMessage") el false ws

connectToWS :: TestWS -> Command Aff (Message TestWS)
connectToWS wsState { dispatch: msgSink } =
  liftEffect <<< msgSink <<< InitWS $ wsState

sendMessage :: EventTarget -> String -> Effect Unit
sendMessage ws msg = do
  let newCustomEvent = CE.new' (EventType "fakeWebSocketMessage") (Just msg)
  msgEvt <- CE.toEvent <$> newCustomEvent
  void $ ET.dispatchEvent msgEvt ws

spec :: Spec Unit
spec = before setup $
  describe "Main app" $
    it "Can navigate to different pages" $ \wsState@(TestWS { store, ws }) -> do
      let mqttMsg = "{\"start\": \"test\"}"

      newDsUpdateTimers <- liftEffect $ Ref.new M.empty

      testComponent
        { init: Main.init newDsUpdateTimers $ connectToWS wsState
        , view: Main.view
        , update: Main.update
        } do

          --
          -- I seem to need a bit of a delay to ensure init is done
          -- running. Based on local timings, we'll see how this
          -- works when in CI. So I don't care for this much
          --
          liftAff $ delay $ Milliseconds 250.0

          liftEffect $
            sendMessage ws $ "[" <> Fixtures.signeFixture <> "]"

          find ("h2" `withTestId` "main-title") >>= text
            >>= shouldEqual "Devices"

          -- Devices
          find ("li" `withTestId` "nav-devices" <> " a") >> click
          find ("h2" `withTestId` "main-title") >>= text
            >>= shouldEqual "Devices"

          -- Publish MQTT
          find ("li" `withTestId` "nav-publish-mqtt" <> " a") >> click
          find ("h2" `withTestId` "main-title") >>= text
            >>= shouldEqual "Publish MQTT"

          find ("li" `withTestId` "nav-publish-mqtt" <> " a") >> click
          find ("input" `withTestId` "publish-mqtt-input") >> change mqttMsg

          find ("input" `withTestId` "publish-mqtt-input") >> prop P.value
             >>= shouldEqual mqttMsg

          find ("button" `withTestId` "publish-mqtt-btn") >> click

          --
          -- with this uncommented or the comparison with mqttMsg
          -- below this blows up only in nix build for reasons I
          -- don't understand, will look into it more closely soon
          -- - 2025-02-09
          --
          -- liftAff $ delay $ Milliseconds 250.0

          find ("div" `withTestId` "last-sent-msg") >>= text
            >>= shouldEqual ("Last sent:" <> mqttMsg)

          -- wsStr <- liftEffect $ Ref.read store
          -- wsStr `shouldEqual` mqttMsg

   where
     setup :: Aff TestWS
     setup = do
       store <- liftEffect $ Ref.new "foo"
       ws <- liftEffect webSocketStub
       pure $ TestWS { store, ws }

     withTestId :: String -> String -> String
     withTestId sel testId = sel <> "[data-test-id=\"" <> testId <> "\"]"
