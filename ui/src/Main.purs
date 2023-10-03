module Main where

import Prelude

import AutomationService.DeviceView as Devices
import AutomationService.Helpers (allElements)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish (Transition, Dispatch, ReactElement, forks, forkVoid, (<|))
import Elmish.HTML.Events as E
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Web.Socket.WebSocket (WebSocket, create)


data Page = Home | Devices | PublishMQTT

derive instance Generic Page _

instance Show Page where
  show = genericShow

pageName :: Page -> String
pageName = case _ of
  Devices -> "Devices"
  Home -> "Home"
  PublishMQTT -> "Publish MQTT"

data Message
  = SetPage Page
  | DeviceMsg Devices.Message
  | InitWS WebSocket
  | PublishMsgChanged String
  | Publish

type State =
  { currentPage :: Page
  , devices :: Devices.State
  , ws :: Maybe WebSocket
  , publishMsg :: String
  }

init :: Transition Message State
init = do
  forks $ \ms -> do
    ws <- liftEffect $ create "ws://localhost:8080" []
    liftEffect $ ms (InitWS ws)

  pure
    { currentPage: Home
    , devices:
      { devices: M.empty
      , selectedDeviceId: Nothing
      }
    , ws: Nothing
    , publishMsg: "{}"
    }

update :: State -> Message -> Transition Message State
update s = case _ of
  SetPage newPage -> pure $ s { currentPage = newPage }

  DeviceMsg deviceMsg ->
    Devices.update s.devices deviceMsg # bimap DeviceMsg (s { devices = _ })

  InitWS ws -> do
    forks $ \ms -> Devices.init ws (ms <<< DeviceMsg)
    pure $ s { ws = Just ws }

  PublishMsgChanged msg -> pure $ s { publishMsg = msg }

  Publish -> do
    forkVoid $ liftEffect $ debug $ "Message to publish: " <> (show s.publishMsg)
    pure s

home :: State -> Dispatch Message -> ReactElement
home _s _dispatch = H.div "" "Hey this is home"

publishMQTT :: State -> Dispatch Message -> ReactElement
publishMQTT _s dispatch =
  H.div "input-group"
  [ H.input_ "form-control publish-mqtt"
    { type: "text"
    , onChange: dispatch <| PublishMsgChanged <<< E.inputText
    }
  , H.button_ "btn btn-outline-secondary"
    { type: "button"
    , onClick: dispatch <| Publish
    }
    "Publish"
  ]

view :: State -> Dispatch Message -> ReactElement
view state@{ currentPage } dispatch =
  H.div "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.h2 "" $ pageName currentPage
  , H.ul "" $ H.fragment $ allElements <#> link
  , page currentPage state dispatch
  ]

  where
    link :: Page -> ReactElement
    link pg = H.li "" $
      H.a_ "" { href: "#", onClick: dispatch <| SetPage pg } $ pageName pg

    page :: Page -> State -> Dispatch Message -> ReactElement
    page pg s dispatch' = case pg of
      Devices -> Devices.view s.devices (dispatch' <<< DeviceMsg)
      Home -> home s dispatch
      PublishMQTT -> publishMQTT s dispatch

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }
