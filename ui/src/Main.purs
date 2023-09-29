module Main where

import Prelude

import AutomationService.DeviceView as Devices
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (debug, info, warn)
import Elmish (Transition, Dispatch, ReactElement, forks, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H

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

type State =
  { currentPage :: Page
  , devices :: Devices.State
  }

init :: Transition Message State
init = do
  forks $ \ms -> Devices.init (ms <<< DeviceMsg)
  pure
    { currentPage: Home
    , devices:
      { devices: M.empty
      , selectedDeviceId: Nothing
      }
    }

update :: State -> Message -> Transition Message State
update s = case _ of
  SetPage newPage -> pure $ s { currentPage = newPage }

  DeviceMsg deviceMsg ->
    Devices.update s.devices deviceMsg # bimap DeviceMsg (s { devices = _ })

home :: State -> Dispatch Message -> ReactElement
home _s _dispatch = H.div "" "Hey this is home"

publishMQTT :: State -> Dispatch Message -> ReactElement
publishMQTT _s _dispatch =
  H.div "input-group"
  [ H.input_ "form-control" { type: "text" }
  , H.button_ "btn btn-outline-secondary" { type: "button" } "Publish"
  ]

view :: State -> Dispatch Message -> ReactElement
view state@{ currentPage } dispatch =
  H.div "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.h2 "" $ pageName currentPage
  , H.ul "" $ H.fragment $ [ Home, Devices, PublishMQTT ] <#> link
  , page currentPage state dispatch
  ]

  where
    link :: Page -> ReactElement
    link pg = H.li "" $
      H.a_ "" { href: "#", onClick: dispatch <| SetPage pg } $ pageName pg

    page :: Page -> State -> Dispatch Message -> ReactElement
    page p s dispatch' = case p of
      Devices -> Devices.view s.devices (dispatch' <<< DeviceMsg)
      Home -> home s dispatch
      PublishMQTT -> publishMQTT s dispatch

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }
