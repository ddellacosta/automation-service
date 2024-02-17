module Main where

import Prelude

import AutomationService.Device (decodeDevices) as Devices
import AutomationService.DeviceView (DeviceStateUpdateTimers, State, initState, update,
                                     view)
  as Devices
import AutomationService.DeviceViewMessage (Message(..)) as Devices
import AutomationService.Helpers (allElements, maybeHtml)
import AutomationService.Message (Message(..), Page(..), pageName, pageNameClass)
import AutomationService.WebSocket (class WebSocket, addWSEventListener, connectToWS, sendString)
import Data.Argonaut (parseJson)
import Data.Bifunctor (bimap)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug)
import Effect.Ref as Ref
import Effect.Ref (Ref)
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
  :: forall ws
   . Ref Devices.DeviceStateUpdateTimers
  -> WebSocket ws
  => Command Aff (Message ws)
  -> Transition (Message ws) (State ws)
init newDsUpdateTimers connectToWS = do
  forks connectToWS
  pure
    { currentPage: Devices
    , devices: Devices.initState newDsUpdateTimers
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

            -- TODO ...see below re: state updates
            -- deviceState = DeviceState.decodeDeviceState =<< jsonBlob

          -- so it seems like we get a device state message as soon 
          -- as we update the device, for every change. some things I 
          -- want
          -- * it should be easy enough to determine if we need to 
          --   update simply by comparing the stored device state 
          --   with the new device state. So, we'll need a place to 
          --   store the device data, which we already have I think
          -- * each device state should be easily comparable _somehow_ 
          --   to the current device state. So, we need some sort of 
          --   abstraction for comparing a device's state to the 
          --   device type's state and effect any relevant change 
          --   that is needed. I guess this is almost like an 
          --   abstraction of the same simply json-diff check we'd do 
          --   first (?)
            
            

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

          --
          -- TODO figure out algorithm for updating device states
          --  that doesn't slow everything to a halt but is
          --  responsive enough to respond quickly to user feedback.
          --
          --
          -- let
          --   deviceStateMsg = either
          --     (\jsonDecodeError ->
          --       Devices.LoadDeviceStateFailed <<< show $ jsonDecodeError)
          --     (\deviceState' -> Devices.LoadDeviceState deviceState')
          --     deviceState

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
      PublishMQTT -> publishMQTT s dispatch
      Devices -> Devices.view s.devices (dispatch <<< DeviceMsg)

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
