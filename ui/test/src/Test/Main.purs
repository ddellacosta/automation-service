module Test.Main where

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Console as Console
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (($), (<>), (=<<), Unit, bind, discard, flip, pure, unit)
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
  , pageLogs :: Ref (Array String)
  }

spec :: Spec Unit
spec = before setup $ after teardown $
  describe "Main app" $
    it "Can navigate to different pages" $ \ctx@{ page } -> do
      outcome <- attempt $ do
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

      case outcome of
        Right _ -> pure unit
        Left err -> do
          logs <- liftEffect $ Ref.read ctx.pageLogs
          html <- PW.content page
          throwError $ error $
            message err
              <> "\n\n--- page console output (most recent last) ---\n"
              <> intercalate "\n" logs
              <> "\n\n--- page HTML at failure (first 3000 chars) ---\n"
              <> String.take 3000 html

  where
    setup :: Aff TestContext
    setup = do
      browser <- PW.launch { headless: true }
      page <- PW.newPage browser

      pageLogsRef <- liftEffect $ Ref.new []

      let
        pageLog line = do
          Console.log ("[page] " <> line)
          Ref.modify_ (_ <> [line]) pageLogsRef

      -- Surface page-side console output, JS errors, and network activity
      -- in the test log, so CI shows what the app was doing (or how it
      -- crashed); they are also collected so test failures can include
      -- them (see the attempt/case in the test body)
      PW.onConsole page pageLog
      PW.onPageError page pageLog
      PW.onResponse page pageLog
      PW.onRequestFailed page pageLog

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

      pure { browser, page, wsRoute: wsRouteRef, sentMessages: sentRef, pageLogs: pageLogsRef }

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
