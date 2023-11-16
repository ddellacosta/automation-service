module Main where

import Prelude

import Data.Argonaut.Encode.Class (encodeJson)
import AutomationService.Device (decodeDevices) as Devices
import AutomationService.DeviceState (decodeDeviceState) as DeviceState
import AutomationService.DeviceView (State, update, view) as Devices
import AutomationService.DeviceViewMessage (Message(..)) as Devices
import AutomationService.Helpers (allElements, maybeHtml)
import AutomationService.Message (Message(..), Page(..), pageName, pageNameClass)
import AutomationService.WebSocket (class WebSocket, addWSEventListener, connectToWS, sendString)
import Data.Argonaut (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (either)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug)
import Elmish (Dispatch, ReactElement, Transition, forks, forkVoid, (<|))
import Elmish.Boot (defaultMain)
import Elmish.Component (Command)
import Elmish.HTML (_data)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign (unsafeFromForeign)
import Web.Event.EventTarget (eventListener)
import Web.Socket.Event.MessageEvent (data_, fromEvent)

type State ws =
  { currentPage :: Page
  , devices :: Devices.State
  , websocket :: Maybe ws
  , publishMsg :: String
  , lastSentMsg :: Maybe String
  }

init
  :: forall ws. WebSocket ws
  => Command Aff (Message ws)
  -> Transition (Message ws) (State ws)
init connectToWS = do
  forks connectToWS

  pure
    { currentPage: Devices
    , devices:
      { devices: M.empty
      , deviceStates: M.empty
      , selectedDeviceId: Nothing
      }
    , websocket: Nothing
    , publishMsg: "{}"
    , lastSentMsg: Nothing
    }

update
  :: forall ws. WebSocket ws
  => State ws
  -> (Message ws)
  -> Transition (Message ws) (State ws)
update s = case _ of
  SetPage newPage -> pure $ s { currentPage = newPage }

  DeviceMsg deviceMsg ->
    Devices.update s.websocket s.devices deviceMsg # bimap DeviceMsg (s { devices = _ })

  InitWS ws -> do
    forks $ \msgSink -> do
      let msgSink' = msgSink <<< DeviceMsg
      el <- liftEffect $ eventListener $ \evt -> do
        for_ (fromEvent evt) \msgEvt -> do
          let
            jsonStr = unsafeFromForeign $ data_ msgEvt
            jsonBlob = parseJson jsonStr
            devices = Devices.decodeDevices =<< jsonBlob
            deviceState = DeviceState.decodeDeviceState =<< jsonBlob

          debug jsonStr

          --
          -- Probably going to abstract this pattern away.
          -- What I really want is something that lets me run a bunch
          -- of Eithers and collect the Left values until I hit a
          -- Right. That is, I want the short-circuiting behavior of
          -- `<|>` with something that returns the accumulated state
          -- of all Left values at the end. And the accumulated state
          -- should tell me in order what it failed at and finally
          -- what it succeeded with. Also I think I only want one
          -- message for parsing failure that summarizes what failed,
          -- and sends it to debug by default, and warns only if
          -- nothing was parsed successfully.
          --

          msgSink' $ either
            (\jsonDecodeError ->
              Devices.LoadDeviceStateFailed <<< show $ jsonDecodeError)
            (\deviceState' -> Devices.LoadDeviceState deviceState')
            deviceState

          msgSink' $ either
            (\jsonDecodeError ->
              Devices.LoadDevicesFailed <<< show $ jsonDecodeError)
            (\devices' -> Devices.LoadDevices devices')
            devices

      liftEffect $ addWSEventListener ws el

    pure $ s { websocket = Just ws }

  PublishMsgChanged msg -> pure $ s { publishMsg = msg }

  Publish -> do
    forkVoid $ do
      liftEffect $ debug $ "Message to publish: " <> (show s.publishMsg)
      for_ s.websocket $ \ws -> liftEffect $ sendString ws $ encodeJson s.publishMsg
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
  ]

view :: forall ws. WebSocket ws => (State ws) -> Dispatch (Message ws) -> ReactElement
view state@{ currentPage } dispatch =
  H.div "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.h2_ ("main-title " <> "main-title-" <> pageNameClass currentPage)
    { _data: _data { "test-id": "main-title" }}
    $ pageName currentPage
  , H.ul "" $ H.fragment $ allElements <#> link
  , page currentPage state
  ]

  where
    link :: Page -> ReactElement
    link pg = H.li_ (pageNameClass pg <> " navlink")
      { _data: _data { "test-id": "nav-" <> pageNameClass pg }
      } $
      H.a_ "" { href: "#", onClick: dispatch <| SetPage pg } $ pageName pg

    page :: WebSocket ws => Page -> State ws -> ReactElement
    page pg s = case pg of
      PublishMQTT -> publishMQTT s dispatch
      Devices -> Devices.view s.devices (dispatch <<< DeviceMsg)

main :: Effect Unit
main = defaultMain
  { def:
      { init: init connectToWS
      , view
      , update
      }
  , elementId: "app"
  }
