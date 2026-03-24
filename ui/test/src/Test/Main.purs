module Test.Main where

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (($), (<>), (=<<), Unit, bind, discard, flip, pure)
import Test.Fixtures as Fixtures
import Test.Playwright as PW
import Test.Playwright.RouteWebSocket as WSRoute
import Test.Spec (Spec, after, before, describe, it)
import Test.Spec.Assertions (shouldEqual)


type TestContext =
  { browser :: PW.Browser
  , page :: PW.Page
  , wsRoute :: Ref (Maybe WSRoute.WebSocketRoute)
  , sentMessages :: Ref (Array String)
  }

spec :: Spec Unit
spec = before setup $ after teardown $
  describe "Main app" $
    it "Can navigate to different pages" $ \ctx@{ page } -> do

      -- Wait for the app's WebSocket connection to be intercepted,
      -- then send it device data as the server
      wsRef <- liftEffect $ Ref.read ctx.wsRoute
      case wsRef of
        Nothing -> throwError (error "WS route not captured")
        Just ws -> liftEffect $ do
          WSRoute.sendToPage ws Fixtures.groupsWithBasementStandingLampFixture

          WSRoute.sendToPage ws $
            "[" <> Fixtures.coordinator <>
            "," <> Fixtures.signeFixture <>
            "," <> Fixtures.basementStandingLampBottomFixture <>
            "," <> Fixtures.basementStandingLampTopFixture <>
            "]"


      -- Devices

      PW.waitForSelector page "h2[data-test-id='main-title']"
      title <- PW.textContent =<< PW.locator page "h2[data-test-id='main-title']"
      title `shouldEqual` Just "Devices"

      PW.click =<< PW.locator page "li[data-test-id='nav-devices'] a"
      devTitle <- PW.textContent =<< PW.locator page "h2[data-test-id='main-title']"
      devTitle `shouldEqual` Just "Devices"

      device1Name <- PW.textContent =<< flip PW.nth 0 =<<
        PW.locator page "div.all-devices div.device .card-body .card-header"
      device1Name `shouldEqual` Just "Basement Black Signe"

      device2Name <- PW.textContent =<< flip PW.nth 1 =<<
        PW.locator page "div.all-devices div.device .card-body .card-header"
      device2Name `shouldEqual` Just "Basement Standing Lamp Bottom"

      device3Name <- PW.textContent =<< flip PW.nth 2 =<<
        PW.locator page "div.all-devices div.device .card-body .card-header"
      device3Name `shouldEqual` Just "Basement Standing Lamp Top"


      -- Groups

      -- this will fail if default_binding_group is present
      groupName <- PW.textContent =<<
        PW.locator page "div.all-devices div.group .card-body .card-header"
      groupName `shouldEqual` Just "Basement Standing Lamp"

      PW.pause page


      -- Publish MQTT

      PW.click =<< PW.locator page "li[data-test-id='nav-publish-mqtt'] a"

      let mqttMsg = "{\"start\": \"test\"}"
      mqttInput <- PW.locator page "input[data-test-id='publish-mqtt-input']"

      PW.fill mqttInput mqttMsg
      PW.click =<< PW.locator page "button[data-test-id='publish-mqtt-btn']"

      lastSent <- PW.textContent =<< PW.locator page "div[data-test-id='last-sent-msg']"
      lastSent `shouldEqual` Just ("Last sent:" <> mqttMsg)

      -- Also verify what the page sent via the WS (the publish msg)
      sent <- liftEffect $ Ref.read ctx.sentMessages

      -- sent will contain the JSON the app sent via sendString/sendJson
      sent `shouldEqual` [
        "{\"topic\":\"zigbee2mqtt/Basement Black Signe\",\"subscribe\":\"HTTP 8850\"}","{\"topic\":\"zigbee2mqtt/Basement Black Signe/get\",\"publish\":{\"state\":\"\"}}","{\"start\": \"test\"}"
        ]

      -- PW.pause page

      teardown ctx

  where
    setup :: Aff TestContext
    setup = do
      browser <- PW.launch { headless: false }
      page <- PW.newPage browser

      -- Refs to capture the route and outgoing messages
      wsRouteRef <- liftEffect $ Ref.new Nothing
      sentRef <- liftEffect $ Ref.new []

      -- Intercept ALL WebSocket connections the page makes.
      WSRoute.routeWebSocket page "**" \ws -> do
        Ref.write (Just ws) wsRouteRef

        -- Capture messages the app sends to the server
        WSRoute.onMessage ws \msg ->
          Ref.modify_ (_ <> [msg]) sentRef

      -- Navigate to the app, make this configurable
      PW.goto page "http://localhost:8850"

      pure { browser, page, wsRoute: wsRouteRef, sentMessages: sentRef }

    teardown :: TestContext -> Aff Unit
    teardown ctx = PW.close ctx.browser
