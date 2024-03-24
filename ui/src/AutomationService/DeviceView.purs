module AutomationService.DeviceView
  ( DeviceStateUpdateTimers
  , State
  , init
  , initState
  , view
  , update
  )
where

import Prelude

import AutomationService.Device as Device
import AutomationService.Device (Device(..), DeviceDetails(..), DeviceId,
                                 Devices, details, deviceTopic, getTopic,
                                 setTopic)
import AutomationService.DeviceState (DeviceState, DeviceStates)
import AutomationService.DeviceViewMessage (Message(..))
import AutomationService.Exposes (Exposes)
import AutomationService.Helpers (maybeHtml)
import AutomationService.MQTT as MQTT
import AutomationService.React.SketchColor (sketchColor)
import AutomationService.WebSocket (class WebSocket, sendJson, sendString)
import Control.Alternative (guard)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (catMaybes, sortBy)
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldr, intercalate)
import Data.List as L
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Console (debug)
import Effect.Ref (Ref)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|), (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign.Object as O

type DeviceStateUpdateTimers = Map DeviceId Instant

type State =
  { devices :: Devices
  , deviceStates :: DeviceStates
  , selectedDeviceId :: Maybe DeviceId
  , deviceStateUpdateTimers :: Ref DeviceStateUpdateTimers
  }

initState :: Ref DeviceStateUpdateTimers -> State
initState dsUpdateTimers =
  { devices: M.empty
  , deviceStates: M.empty
  , selectedDeviceId: Nothing
  , deviceStateUpdateTimers: dsUpdateTimers
  }

init :: Ref DeviceStateUpdateTimers -> Transition Message State
init = pure <<< initState

update :: forall ws. WebSocket ws => Maybe ws -> State -> Message -> Transition Message State
update ws s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ debug $ "loaded devices: " <> show newDevices
    forkVoid $ do
      liftEffect $ for_ newDevices $ \d -> do
        let
          -- this needs to get passed in from parent state as config, or something
          subscribeMsg = MQTT.subscribe (deviceTopic (_.name <<< details $ d)) "HTTP 8080"
          pingStateMsg = MQTT.publish (getTopic (_.name <<< details $ d)) $ MQTT.state ""
        debug $ "subscribing with: " <> (stringify $ encodeJson subscribeMsg)
        debug $ "pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)
        for_ ws $ \ws' -> do
          sendJson ws' <<< encodeJson $ subscribeMsg
          sendJson ws' <<< encodeJson $ pingStateMsg
    pure $
      s { devices =
             foldr
               (\d ds -> M.insert (_.id <<< details $ d) d ds)
               M.empty
               newDevices
        }

  -- Currently unimplemented on the WS end
  LoadDeviceState deviceState -> do
    forkVoid $ liftEffect $ do
      debug $ "loaded DeviceState: " <> show deviceState
    pure $
      s { deviceStates =
             M.insert deviceState.device.ieeeAddr deviceState s.deviceStates
        }

  LoadDevicesFailed _msg ->
    -- (forkVoid $ liftEffect $ debug $ "LoadDevicesFailed with msg: " <> msg) *> pure s
    pure s

  LoadDeviceStateFailed _msg ->
    -- (forkVoid $ liftEffect $ debug $ "LoadDeviceStateFailed with msg: " <> msg) *> pure s
    pure s

  DeviceSelected deviceId -> do
    forkVoid $ liftEffect $ debug $ "device: " <> deviceId
    pure $ s { selectedDeviceId = Just deviceId }

  NoDeviceSelected -> do
    forkVoid $ liftEffect $ debug $ "unselecting any device"
    pure $ s { selectedDeviceId = Nothing }

  PublishDeviceMsg msg -> do
    forkVoid $ liftEffect $ do
      debug $ "Publishing with '" <> msg  -- <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' msg 
    pure s

view :: State -> Dispatch Message -> ReactElement
view { devices, deviceStates, selectedDeviceId } dispatch =
  H.div "" -- "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.select_
      "device-select"
      { onChange: dispatch <| \e -> case E.selectSelectedValue e of
           "" -> NoDeviceSelected
           deviceId -> DeviceSelected deviceId
      } $
      [ H.option_ "" { value: "" } "No Device Selected" ]
      <>
      (sortBy (\a b -> compare (_.name <<< details $ a) (_.name <<< details $ b)) devicesA <#> \d ->
        H.option_ "" { value: _.id <<< details $ d } $ _.name <<< details $ d
      )

  , maybeHtml (flip M.lookup devices =<< selectedDeviceId) $
    listDevice (flip M.lookup deviceStates =<< selectedDeviceId)

  ]

  where
    -- This is converted to an Array first because there is no
    -- instance of Elmish.React.ReactChildren (List ReactElement)
    -- in a few places where I want to map over devices to produce
    -- HTML output...
    -- ...maybe there's a better way?
    devicesA :: Array Device
    devicesA = L.toUnfoldable $ M.values devices

    listDevice mDeviceState = case _ of
      (OnOffLight deviceDetails) ->
        listDevice' mDeviceState deviceDetails
      (WindowCovering deviceDetails) ->
        listDevice' mDeviceState deviceDetails
      _ ->
        H.div "card mt-2"
        [ H.div "card-body"
          [ H.p "" "hey"
          ]
        ]

    listDevice' mDeviceState { id, name, category, model, manufacturer, exposes } =
      H.div "card mt-2"
      [ H.div "card-body"
        [ H.h4 "" name
        , H.ul ""
          [ H.li "" $ "id: " <> id
          , H.li "" $ "category: " <> category
          , maybeHtml model $ \model' -> H.li "" $ "model: " <> model'
          , maybeHtml manufacturer $ \m -> H.li "" $ "manufacturer: " <> m
          , H.li "" $
              listExposes
                mDeviceState
                { id, name, category, model, manufacturer }
                exposes
          ]
        ]
      ]

    listExposes
      :: Maybe DeviceState
      -> { id :: String
         , name :: String
         , category :: String
         , model :: Maybe String
         , manufacturer :: Maybe String
         }
      -> Array Exposes
      -> ReactElement
    listExposes ds s allExposes =
      H.div "" $
      [ H.div "" $ "name: " <> s.name
      , H.div "" $ "exposes: " <> (show allExposes)
      ]

--      <>
--      (exposes <#> case e.subProps of
--          Binary sp -> binary ds s e
--          Enum sp -> enum ds s e
--          Numeric sp -> numeric ds s e
--          Composite sp -> composite ds s e
--          List sp -> list ds s e
--          Null -> generic ds e ""
--      )
--
--    binary
--      :: forall r. Maybe DeviceState
--      -> { name :: String | r}
--      -> CapabilityBase BinaryProps
--      -> ReactElement
--    binary ds s cap
--      | not (canSet cap.access) = H.div "" $ H.text "hey"
--      | otherwise =
--        H.div_ "form-check form-switch" {}
--        [ H.input_
--          "form-check-input"
--          { type: "checkbox"
--          , role: "switch"
--          , id: "flexSwitchCheckDefault"
--          , checked:
--              case (fromMaybe (ValueOnOffString "OFF") $ _.state =<< ds) of
--                isChecked |
--                  ValueOnOffString "ON" == isChecked -> true
--                _ -> false
--
--                -- I should test cap.property, but probably in the guard? 
--          , onChange: dispatch <| \_e ->
--              PublishDeviceMsg $
--                MQTT.mkPublishMsg (setTopic s.name) $ MQTT.state "TOGGLE"
--          }
--        , H.label_
--          "form-check-label"
--          { htmlFor: "flexSwitchCheckDefault" } $
--          H.text $ "set state for " <> cap.name
--        ]
--
--    enum
--      :: forall r. Maybe DeviceState
--      -> { name :: String | r}
--      -> CapabilityBase EnumProps
--      -> ReactElement
--    enum ds s cap
--      | not (canSet cap.access) = H.div "" $ H.text "hey"
--      | otherwise =
--        H.div "border rounded p-2 m-2"
--        [ H.strong "" cap.name
--        , H.select_
--            "form-select"
--            -- how with Elmish? aria-label="Default select example"
--            { onChange: dispatch
--                <|  PublishDeviceMsg
--                <<< MQTT.mkGenericPublishMsg (setTopic s.name) (fromMaybe "CHECK_YOUR_PROPERTY" cap.property)
--                <<< E.selectSelectedValue
--             }
--            $ cap.values <#> \v -> H.option_ "" { value: v } v
--        , H.p "" $ H.text $ fromMaybe "" cap.description
--        , H.p "" $ H.text $ listAccess cap.access
--        , generic ds cap ""
--        ]
--
--    numeric
--      :: forall r
--       . Maybe DeviceState
--      -> { id :: DeviceId | r }
--      -> Exposes
--      -> ReactElement
--    numeric ds s exposes = case cap.property of
--      Just propName ->
--        let
--          idStr = "numericRange_" <> s.id
--
--          getProp :: String -> DeviceState -> Maybe Int
--          getProp propName' ds' = case propName' of
--            "brightness" -> ds'.brightness
--            "color_temp" -> ds'.colorTemp
--            "color_temp_startup" -> ds'.colorTempStartup
--            _ -> Nothing
--
--          showMaybe alt = show <<< fromMaybe alt
--
--        in
--          H.div ""
--          [ H.label_ "form-label" { htmlFor: idStr } $
--              H.text cap.name
--    
--          , H.input_
--            "form-range"
--            { type: "range"
--            , min: showMaybe 0 cap.valueMin
--            , max: showMaybe 255 cap.valueMax
--            , step: showMaybe 1 cap.valueStep
--            , value: showMaybe 100 (getProp propName =<< ds)
--            , id: idStr
--            , onChange: dispatch <?| \e ->
--                 let
--                   newValue = E.inputText e
--                 in
--                   ds >>= \ds' ->
--                     Just $ PublishDeviceMsg $
--                       MQTT.mkGenericPublishMsg
--                         (setTopic ds'.device.friendlyName) propName newValue
--            }
--  
--          , generic ds cap ""
--          ]
--
--      Nothing -> generic ds cap ""
--
--    -- these two are...under-implemented for now
--    composite
--      :: forall s. Maybe DeviceState
--      -> s
--      -> CapabilityBase CompositeProps
--      -> ReactElement
--    composite ds _s cap
--      | cap.property == Just "color" =
--        H.div ""
--        [ sketchColor
--          { onChange: dispatch <?| \color -> do
--               hex <- O.lookup "hex" color
--               topic <- setTopic <<< _.device.friendlyName <$> ds
--               pure <<< PublishDeviceMsg $ MQTT.mkPublishMsg topic $ MQTT.hexColor hex
--          }
--        , generic ds cap $ ", features: " <> show cap.features
--        ]
--      | otherwise =
--        generic ds cap $ ", features: " <> show cap.features
--
--    list
--      :: forall s. Maybe DeviceState
--      -> s
--      -> CapabilityBase ListProps
--      -> ReactElement
--    list ds _s cap =
--      generic ds cap $ ", item_type: " <> show cap.itemType
--
--    generic
--      :: forall r. Maybe DeviceState
--      -> CapabilityBase r
--      -> String
--      -> ReactElement
--    generic _ds cap capFieldsStr =
--      H.div "" $
--      [ H.text $ "name: " <> cap.name
--        <> ", description: " <> fromMaybe "" cap.description
--        <> ", type: " <> cap.capType
--        <> ", feature type: " <> fromMaybe "n/a" cap.featureType
--        <> ", label: " <> cap.label
--        <> ", property: " <> show cap.property
--        <> ", access: " <> listAccess cap.access
--        <> capFieldsStr
--      ]
--
--    listAccess :: Int -> String
--    listAccess a = intercalate ", " $
--      catMaybes
--      [ guard (isPublished a) *> Just "published"
--      , guard (canSet a) *> Just "set"
--      , guard (canGet a) *> Just "get"
--      ]
