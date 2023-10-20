module Main where

import Prelude

import AutomationService.DeviceView as Devices
import AutomationService.Helpers (allElements)
import AutomationService.Message (Message(..), Page(..), pageName, pageNameClass)
import AutomationService.WebSocket (class WebSocket, connectToWS, initializeListeners, sendString)
import Data.Bifunctor (bimap)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish (Dispatch, ReactElement, Transition, forks, forkVoid, (<|))
import Elmish.Boot (defaultMain)
import Elmish.Component (Command)
import Elmish.HTML (_data)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H


type State ws =
  { currentPage :: Page
  , devices :: Devices.State
  , websocket :: Maybe ws
  , publishMsg :: String
  }

init
  :: forall ws. WebSocket ws
  => Command Aff (Message ws)
  -> Transition (Message ws) (State ws)
init connectToWS = do
  forks connectToWS

  pure
    { currentPage: Home
    , devices:
      { devices: M.empty
      , selectedDeviceId: Nothing
      }
    , websocket: Nothing
    , publishMsg: "{}"
    }

update
  :: forall ws. WebSocket ws
  => State ws
  -> (Message ws)
  -> Transition (Message ws) (State ws)
update s = case _ of
  SetPage newPage -> pure $ s { currentPage = newPage }

  DeviceMsg deviceMsg ->
    Devices.update s.devices deviceMsg # bimap DeviceMsg (s { devices = _ })

  InitWS ws -> do
    forks $ \ms -> initializeListeners ws (ms <<< DeviceMsg)
    pure $ s { websocket = Just ws }

  PublishMsgChanged msg -> pure $ s { publishMsg = msg }

  Publish -> do
    forkVoid $ do
      liftEffect $ debug $ "Message to publish: " <> (show s.publishMsg)
      for_ s.websocket $ \ws -> liftEffect $ sendString ws s.publishMsg
    pure s

home :: forall s ws. s -> Dispatch (Message ws) -> ReactElement
home _s _dispatch = H.div "" "Hey this is home"

publishMQTT
  :: forall ws r. WebSocket ws
  => { websocket :: Maybe ws | r }
  -> Dispatch (Message ws)
  -> ReactElement
publishMQTT _s dispatch =
  H.div "input-group"
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
      Home -> home s dispatch
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
