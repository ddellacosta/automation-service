module Main where

import AutomationService.Device (Decoded(..), DecodedStatus(..))
import AutomationService.Device (decodeDevices, mkFailedParse) as Devices
import AutomationService.DeviceMessage (Message(..)) as Devices
import AutomationService.DeviceState as DeviceState
import AutomationService.DeviceView (DeviceStateUpdateTimers, State, initState, update, view) as Devices
import AutomationService.Group (decodeGroups) as Groups
import AutomationService.GroupView (view) as Groups
import AutomationService.Helpers (allElements, maybeHtml)
import AutomationService.Logging (LogLevel(..), debug, warn)
import AutomationService.Logging as Logging
import AutomationService.Message (Message(..), Page(..), pageName, pageNameClass)
import AutomationService.Message as Page
import AutomationService.WebSocket (class WebSocket, addWSEventListener, connectToWS, sendString)
import Control.Monad (when)
import Data.Argonaut (parseJson)
import Data.Array (null)
import Data.Bifunctor (bimap)
import Data.Either (either, fromRight)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Elmish (Dispatch, ReactElement, Transition, forks, forkVoid, (<|))
import Elmish.Boot (defaultMain)
import Elmish.Component (Command)
import Elmish.HTML (_data)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Prelude (Unit, ($), (#), (<>), (<<<), (<$>), (<#>), (=<<), bind, discard, not, pure, show)

type State ws =
  { currentPage :: Page
  , devices :: Devices.State
  , publishMsg :: String
  , lastSentMsg :: Maybe String
  , websocket :: Maybe ws
  }

init
  :: forall ws
   . Ref Devices.DeviceStateUpdateTimers
  -> WebSocket ws
  => Command Aff (Message ws)
  -> Transition (Message ws) (State ws)
init newDsUpdateTimers connectToWS = do
  -- TODO make LogLevel an argument to the application
  forkVoid $ liftEffect $ Logging.setLogLevel Warn
  forks connectToWS
  pure
    { currentPage: Devices
    , devices: Devices.initState newDsUpdateTimers
    , publishMsg: "{}"
    , lastSentMsg: Nothing
    , websocket: Nothing
    }

update
  :: forall ws. WebSocket ws
  => State ws
  -> (Message ws)
  -> Transition (Message ws) (State ws)
update s = case _ of
  SetPage newPage -> do
    pure $ s { currentPage = newPage }

  DeviceMsg deviceMsg -> do
    Devices.update s.websocket s.devices deviceMsg #
      bimap DeviceMsg (s { devices = _ })

  InitWS ws -> do
    forks $ \{ dispatch: msgSink } -> do
      let
        msgSink' = msgSink <<< DeviceMsg
        messageHandler = \msgStr -> do
          let
            jsonBlob = parseJson msgStr
            devices = fromRight (Devices.mkFailedParse jsonBlob) $
                        Devices.decodeDevices <$> jsonBlob
            deviceState = DeviceState.decodeDeviceState =<< jsonBlob
            groups = case devices of
              Decoded DecodingSucceeded devices' ->
                Groups.decodeGroups devices'.devices =<< jsonBlob
              _ ->
                Groups.decodeGroups s.devices.devices =<< jsonBlob

          liftEffect $ do
            debug $ show devices
            debug msgStr

          -- okay, one thing that is needed is a ux for when state hasn't
          -- loaded yet

          msgSink' $ either
            (Devices.LoadDeviceStateFailed <<< show)
            Devices.LoadDeviceState
            deviceState

          -- make appropriate noise about device decoding failures
          case devices of
            Decoded NoDevicesDecoded { errors } -> do
              -- If we don't have any devices then it's likely any
              -- errors are going to be obviously because we were
              -- trying to decode the wrong type of JSON (i.e.
              -- DeviceState or Groups JSON), so we keep the log
              -- level at debug to avoid a ton of noise:
              liftEffect <<< debug $ "Devices are empty, errors are " <> show errors

            -- DecodingSucceeded implies we have at least one device
            Decoded DecodingSucceeded { devices: devices', errors } -> do
              -- log any decoding failures when we do have devices,
              -- as we know these should be about devices that we
              -- aren't decoding properly
              when (not null errors) $
                liftEffect <<< warn $ "Decoding errors: " <> show errors
              msgSink' <<< Devices.LoadDevices $ devices'

            Decoded DecodingFailed { errors } -> do
              liftEffect <<< warn $ "Decoding failed: " <> show errors
              msgSink' <<< Devices.LoadDevicesFailed <<< show $ errors

            Decoded BadJson { errors } -> do
              liftEffect <<< debug $ "Bad JSON: " <> show errors

          msgSink' $ either
            (Devices.LoadGroupsFailed <<< show)
            Devices.LoadGroups
            groups

      liftEffect $ addWSEventListener ws messageHandler

    pure $ s { websocket = Just ws }

  PublishMsgChanged msg -> do
    pure $ s { publishMsg = msg }

  Publish -> do
    forkVoid $ do
      liftEffect $ debug $ "Message to publish: " <> s.publishMsg
      for_ s.websocket $ \ws -> liftEffect $ sendString ws s.publishMsg
    pure $ s { lastSentMsg = Just s.publishMsg }

publishMQTT
  :: forall ws r. WebSocket ws
  => { lastSentMsg :: Maybe String
     , websocket :: Maybe ws
     | r
     }
  -> Dispatch (Message ws)
  -> ReactElement
publishMQTT s dispatch =
  H.div "publish-mqtt" $
  [ H.div "input-group"
    [ H.input_ "form-control publish-mqtt"
      { type: "text"
      , _data: _data { "test-id": "publish-mqtt-input" }
      , onChange: dispatch <| PublishMsgChanged <<< E.inputText
      }
    , H.button_ "btn btn-outline-secondary"
      { _data: _data { "test-id": "publish-mqtt-btn" }
      , onClick: dispatch <| Publish
      }
      "Publish"
    ]

  , maybeHtml s.lastSentMsg $ \msg ->
      H.div_
        "publish-mqtt-status border border-primary-subtle rounded m-2 p-3 mt-4 text-secondary small"
        { _data: _data { "test-id": "last-sent-msg" }} $
        H.div ""
        [ H.div "text-info fst-italic text-opacity-50 mb-0" "Last sent:"
        , H.div "mt-0" msg
        ]

  , H.div "mt-4 p-3 border border-info text-info bg-dark rounded"
    [ H.h5 "" "Examples"
    , H.code "" "{\"publish\": {\"start\": \"basementMirrorLight\"}, \"topic\": \"automation-service/set\"}" 
    ]
  ]

view :: forall ws. WebSocket ws => (State ws) -> Dispatch (Message ws) -> ReactElement
view state@{ currentPage } dispatch =
  H.div "container mx-auto mt-2 d-flex flex-column justify-content-between"
  [ H.ul "mb-2 list-group list-group-horizontal" $
      H.fragment $ allElements <#> link

  , H.h2_ ("main-title " <> "main-title-" <> pageNameClass currentPage)
    { _data: _data { "test-id": "main-title" }}
    $ pageName currentPage

  , page currentPage state

  ]

  where
    link :: Page -> ReactElement
    link pg = H.li_ (pageNameClass pg <> " navlink list-group-item")
      { _data: _data { "test-id": "nav-" <> pageNameClass pg }
      } $
      H.a_ "" { href: "#", onClick: dispatch <| SetPage pg } $ pageName pg

    page :: WebSocket ws => Page -> State ws -> ReactElement
    page pg s = case pg of
      Page.PublishMQTT ->
        publishMQTT
          { lastSentMsg: s.lastSentMsg
          , websocket: s.websocket
          }
          dispatch

      Page.Devices -> Devices.view s.devices $ dispatch <<< DeviceMsg

      Page.Groups -> Groups.view s.devices $ dispatch <<< DeviceMsg


main :: Effect Unit
main = do
  newDsUpdateTimers <- Ref.new M.empty
  defaultMain
    { def:
      { init: init newDsUpdateTimers connectToWS
      , view
      , update
      }
    , elementId: "app"
    }
