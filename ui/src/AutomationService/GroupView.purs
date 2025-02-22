module AutomationService.GroupView
  ( view
  )
where

import AutomationService.DeviceView (State) as Devices
import AutomationService.DeviceMessage (Message) as Devices
-- import AutomationService.MQTT as MQTT
import AutomationService.React.Bootstrap as Bootstrap
import Data.Array as Array
-- import Data.Maybe (Maybe)
import Elmish (Dispatch, ReactElement) -- , (<|), (<?|))
import Elmish.HTML.Styled as H
import Prelude (($), (<<<), (<>), (<#>), show)

view :: Devices.State -> Dispatch Devices.Message -> ReactElement
view _state@{ groups } _dispatch =
  H.div ""
  [ H.div "" $ H.text $ (show $ Array.length groups) <> " Groups"
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
