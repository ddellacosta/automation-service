module Test.Main where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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

      -- (fixture data is fed to the app as the "server" from inside the
      -- WS route handler in setup, to avoid racing the app's connection)

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

      -- The app's startup protocol, in order:
      --
      --   1. when the groups fixture arrives (before any devices are
      --      known), it subscribes + pings each group
      --   2. when the devices fixture arrives, it subscribes + pings each
      --      device (iteration order is Data.Map order, i.e. by device id)
      --   3. groups are then re-loaded (ReLoadGroups) so group members can
      --      merge in the newly-arrived device info, and loadGroups
      --      re-subscribes + re-pings as part of that (redundant but
      --      harmless — MQTT subscribe is idempotent)
      --
      -- followed by the message published by the Publish MQTT page test.
      sent `shouldEqual`
        [ subscribeMsg "zigbee2mqtt/Basement Standing Lamp"
        , pingMsg "zigbee2mqtt/Basement Standing Lamp"
        , subscribeMsg "zigbee2mqtt/Basement Standing Lamp Bottom"
        , pingMsg "zigbee2mqtt/Basement Standing Lamp Bottom"
        , subscribeMsg "zigbee2mqtt/Basement Black Signe"
        , pingMsg "zigbee2mqtt/Basement Black Signe"
        , subscribeMsg "zigbee2mqtt/Basement Standing Lamp Top"
        , pingMsg "zigbee2mqtt/Basement Standing Lamp Top"
        , subscribeMsg "zigbee2mqtt/Basement Standing Lamp"
        , pingMsg "zigbee2mqtt/Basement Standing Lamp"
        , "{\"start\": \"test\"}"
        ]

      teardown ctx

  where
    setup :: Aff TestContext
    setup = do
      browser <- PW.launch { headless: true }
      page <- PW.newPage browser

      -- Refs to capture the route and outgoing messages
      wsRouteRef <- liftEffect $ Ref.new Nothing
      sentRef <- liftEffect $ Ref.new []

      -- Intercept ALL WebSocket connections the page makes. As the
      -- "server" side of the intercepted connection, feed the app its
      -- fixture data right away. This avoids racing the app's connect:
      -- the route handler fires when the app attempts the connection,
      -- and the app attaches its message handler synchronously in the
      -- same tick it creates the socket, so it is guaranteed to receive
      -- whatever we send from here.
      WSRoute.routeWebSocket page "**" \ws -> do
        Ref.write (Just ws) wsRouteRef

        -- Capture messages the app sends to the server
        WSRoute.onMessage ws \msg ->
          Ref.modify_ (_ <> [msg]) sentRef

        -- Send device/group data as the server
        WSRoute.sendToPage ws Fixtures.groupsWithBasementStandingLampFixture
        WSRoute.sendToPage ws $
          "[" <> Fixtures.coordinator <>
          "," <> Fixtures.signeFixture <>
          "," <> Fixtures.basementStandingLampBottomFixture <>
          "," <> Fixtures.basementStandingLampTopFixture <>
          "]"

      -- Navigate to the app, make this configurable
      PW.goto page "http://localhost:8850"

      pure { browser, page, wsRoute: wsRouteRef, sentMessages: sentRef }

    teardown :: TestContext -> Aff Unit
    teardown ctx = PW.close ctx.browser

    -- message shapes emitted by the app (see DeviceView.purs): the
    -- subscription name is derived from the port of the app's WS URL
    subscribeMsg :: String -> String
    subscribeMsg topic =
      "{\"topic\":\"" <> topic <> "\",\"subscribe\":\"HTTP 8850\"}"

    pingMsg :: String -> String
    pingMsg topic =
      "{\"topic\":\"" <> topic <> "/get\",\"publish\":{\"state\":\"\"}}"
