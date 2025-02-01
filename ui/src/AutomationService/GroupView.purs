module AutomationService.GroupView
  ( State
  , init
  , initState
  , update
  , view
  )
where

import AutomationService.Device as Device
import AutomationService.DeviceView as Devices
import AutomationService.Group (Group)
import AutomationService.GroupMessage (Message(..))
import AutomationService.Logging (debug)
import AutomationService.MQTT as MQTT
import AutomationService.React.Bootstrap as Bootstrap
import AutomationService.WebSocket (class WebSocket, sendJson, sendString)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array as A
import Data.Foldable (for_)
-- import Data.Map as M
import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Elmish (Transition, Dispatch, ReactElement, forkVoid) -- , (<|), (<?|))
import Elmish.HTML.Styled as H
import Prelude (($), (<<<), (<>), (<#>), discard, pure, show)

-- import Debug (traceM)


type State =
  { groups :: Array Group
  }

initState :: State
initState =
  { groups: []
  }

init :: Transition Message State
init = pure initState

update
  :: forall ws. WebSocket ws
  => Maybe ws
  -> State
  -> Message
  -> Transition Message State
update ws s = case _ of
  LoadGroups newGroups -> do
    -- _ <- traceM newGroups

    forkVoid $ liftEffect $ debug $ "loaded groups: " <> show newGroups

    forkVoid $ do
      liftEffect $ for_ newGroups $ \g -> do
        let
          -- this needs to get passed in from parent state as config, or something
          subscribeMsg =
            MQTT.subscribe (Device.deviceTopic g.name) "HTTP 8080"
          pingStateMsg =
            MQTT.publish (Device.getTopic g.name) $ MQTT.state ""

        debug $ "Groups: subscribing with: " <> (stringify $ encodeJson subscribeMsg)
        debug $
          "Groups: pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)

        for_ ws $ \ws' -> do
          sendJson ws' <<< encodeJson $ subscribeMsg
          sendJson ws' <<< encodeJson $ pingStateMsg

    pure s { groups = newGroups }

  LoadGroupsFailed _msg -> do
    -- forkVoid $
    --   liftEffect $ debug $ "LoadGroupsFailed with msg: " <> msg) *> pure s
    -- _ <- traceM msg
    pure s

  PublishGroupMsg msg -> do
    forkVoid $ liftEffect $ do
      debug $ "Publishing with '" <> msg  -- <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' msg 
    pure s

view :: Devices.State -> State -> Dispatch Message -> ReactElement
view _state { groups } _dispatch =
  H.div ""
  [ H.div "" $ H.text $ (show $ A.length groups) <> " Groups"
  , H.ul "" $ groups <#> \g ->
     Bootstrap.accordion {} $
       Bootstrap.accordionItem { eventKey: 0 }
       [ Bootstrap.cardHeader {} $
         H.h4 "" g.name
--         [ Devices.onOffSwitch
--           (getDeviceState deviceStates =<< M.lookup m.id devices)

       , Bootstrap.cardBody {} $
         [
           H.div "" $ H.text "oh hey here is something"
         ]
       ] 

--      H.li "" $
--        H.text $ g.name <> " - " <> (show g.id)
--        <> (show $ g.members <#> \m -> M.lookup m.id devices)
  ]

  where
    -- onOffSwitch :: Maybe DeviceState -> DeviceDetails -> ReactElement
    _onOffSwitch :: ReactElement
    _onOffSwitch =
      H.div "" $ H.text "heyyyyy"

--       case getOnOffSwitch device of
--         Nothing ->
--           H.div "" $ H.text "Not a light?"
-- 
--         Just cap
--           | not (canSet cap.access) ->
--             H.div "" $ H.text "Not allowed to turn this one on chief"
-- 
--           | otherwise ->
--             H.div "form-check form-switch"
--             [ H.input_
--               "form-check-input"
--               { type: "checkbox"
--               , role: "switch"
--               , id: "flexSwitchCheckDefault"
--               , checked:
--                 case mDeviceState >>= _.state of
--                   Just onOffValue -> isOn cap onOffValue
--                   _ -> false
--               , onChange: dispatch <| \_e ->
--                 PublishDeviceMsg $
--                 MQTT.mkPublishMsg (Device.setTopic device.name) $ MQTT.state "TOGGLE"
--               }
-- 
--             , H.label_
--               "form-check-label"
--               { htmlFor: "flexSwitchCheckDefault" } $
--               H.empty
--             ]
