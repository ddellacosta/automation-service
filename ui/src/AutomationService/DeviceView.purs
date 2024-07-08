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

import AutomationService.Device (Device(..), DeviceDetails, DeviceId, Devices,
                                 details, deviceTopic, getTopic, setTopic)
import AutomationService.DeviceState (DeviceState, DeviceStates)
import AutomationService.DeviceViewMessage (Message(..))
import AutomationService.Exposes (Exposes, canSet, enumValues, isOn)
import AutomationService.Lighting (ColorSetter(..), getColorSetter, getOnOffSwitch,
                                   getPreset)
import AutomationService.Helpers (maybeHtml)
import AutomationService.MQTT as MQTT
import AutomationService.React.ColorWheel (colorWheel)
import AutomationService.WebSocket (class WebSocket, sendJson, sendString)
import Color as Color
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (drop, sortBy, take)
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldr)
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
  [ H.div "" $ H.text $ "Device count: " <> (show $ L.length $ M.values devices)
  , H.select_
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

  , H.fragment $
      deviceSummaryRows [] $
        (\d -> deviceSummary (getDeviceState deviceStates d) d) <$> devicesA
  ]

  where
    -- This is converted to an Array first because there is no
    -- instance of Elmish.React.ReactChildren (List ReactElement)
    -- in a few places where I want to map over devices to produce
    -- HTML output...
    -- ...maybe there's a better way?
    devicesA :: Array Device
    devicesA = L.toUnfoldable $ M.values devices

    deviceSummaryRows
      :: Array ReactElement
      -> Array ReactElement
      -> Array ReactElement
    deviceSummaryRows elems [] = elems
    deviceSummaryRows elems summaries =
      deviceSummaryRows
        (elems <> [ H.div "row" $ take 3 summaries])
        (drop 3 summaries)

    getDeviceState :: DeviceStates -> Device -> Maybe DeviceState
    getDeviceState deviceStates' = flip M.lookup deviceStates' <<< _.id <<< details

    listDevice mDeviceState = case _ of
      (OnOffLight deviceDetails) ->
        listDevice' mDeviceState deviceDetails
      (WindowCovering deviceDetails) ->
        listDevice' mDeviceState deviceDetails
      (UnknownDevice deviceDetails) ->
        listDevice' mDeviceState deviceDetails
      _ ->
        H.div "card mt-2"
        [ H.div "card-body"
          [ H.p "" "hey"
          ]

        ]

    deviceSummary :: Maybe DeviceState -> Device -> ReactElement
    deviceSummary mDeviceState device =
      H.div "col" $
      H.div "card mt-2"
      [ H.div "card-body text-bg-light p-3"
        [ H.div "card-title" $ _.name <<< details $ device
          -- I don't know why, but this `case` seems to mess up
          -- emacs's PureScript parsing from this point onward
        , case device of
          (ExtendedColorLight deviceDetails) ->
            H.fragment
            [ onOffSwitch mDeviceState deviceDetails
            , enumSelector "effect" mDeviceState deviceDetails
            , colorSelector mDeviceState deviceDetails
            ]

          (ColorTemperatureLight deviceDetails) ->
            H.fragment
            [ onOffSwitch mDeviceState deviceDetails
            , enumSelector "effect" mDeviceState deviceDetails
            ]

          (DimmableLight deviceDetails) ->
            H.fragment
            [ onOffSwitch mDeviceState deviceDetails
            ]

          (OnOffLight deviceDetails) ->
            H.fragment
            [ onOffSwitch mDeviceState deviceDetails
            ]

          (WindowCovering _deviceDetails) -> H.fragment []

          (UnknownDevice _deviceDetails) -> H.fragment []
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
      -> Exposes
      -> ReactElement
    listExposes _ds s allExposes =
      H.div "" $
      [ H.div "" $ "name: " <> s.name
      , H.div "" $ "exposes: " <> (show allExposes)
      ]

    onOffSwitch :: Maybe DeviceState -> DeviceDetails -> ReactElement
    onOffSwitch mDeviceState device =
      case getOnOffSwitch device of
        Nothing ->
          H.div "" $ H.text "Not a light?"

        Just cap
          | not (canSet cap.access) ->
            H.div "" $ H.text "Not allowed to turn this one on chief"

          | otherwise ->
            H.div_ "form-check form-switch" {}
            [ H.input_
              "form-check-input"
              { type: "checkbox"
              , role: "switch"
              , id: "flexSwitchCheckDefault"
              , checked:
                case mDeviceState >>= _.state of
                  Just onOffValue -> isOn cap onOffValue
                  _ -> false
              , onChange: dispatch <| \_e ->
                  PublishDeviceMsg $
                    MQTT.mkPublishMsg (setTopic device.name) $ MQTT.state "TOGGLE"
              }

            , H.label_
              "form-check-label"
              { htmlFor: "flexSwitchCheckDefault" } $
              H.text $ "on/off"
            ]

    colorSelector :: Maybe DeviceState -> DeviceDetails -> ReactElement
    colorSelector _mDeviceState device =
      case getColorSetter device of
        Nothing ->
          H.div "" $ H.text "this doesn't support color selection"

        Just (XYSetter _) ->
          H.div ""
          [ colorWheel
            { onChange: dispatch <?| \color -> do
                --
                -- yeah had problems reading rgb/rgba (NaN everywhere)
                -- and hex/hexa (everything was 0.0) so parsing hs
                -- from hsv and using color to generate hex string
                --
                -- thinking I wrote the EffectFn1 sig wrong for
                -- onChange in colorWheel, maybe should try Foreign,
                -- or Json?
                --
                --
                hsv <- O.lookup "hsv" color
                h <- O.lookup "h" hsv
                s <- O.lookup "s" hsv
                let
                  topic = setTopic device.name
                  -- 0.5 for lightness was a guess, I don't
                  -- understand hsl yet
                  color' = Color.hsl h s 0.5
                pure $
                  PublishDeviceMsg $
                    MQTT.mkPublishMsg topic $
                      MQTT.hexColor $ Color.toHexString color'
            }
          ]

        Just (HueSatSetter _) ->
          H.div ""
          [ colorWheel
            { onChange: dispatch <?| \color -> do
                --
                -- All of these values are available coming back from
                -- colorWheel according to a key dump, at least:
                --
                -- ["rgb","hsl","hsv","rgba","hsla","hsva","hex","hexa"]
                --
                -- hsl and hsv both seem to barf when I try to read the
                -- 'l' and 'v' values respectively, and hsl for some
                -- reason doesn't seem to even populate the saturation
                -- ¯\_(ツ)_/¯
                --
                hsv <- O.lookup "hsv" color
                h <- O.lookup "h" hsv
                s <- O.lookup "s" hsv
                let topic = setTopic device.name
                pure $ PublishDeviceMsg $
                  MQTT.mkPublishMsg topic $ MQTT.hslColor { h, s }
            }
          ]

    enumSelector :: String -> Maybe DeviceState -> DeviceDetails -> ReactElement
    enumSelector presetName _deviceState device =
      case getPreset presetName device of
        Nothing ->
          H.div "" $ H.text "oh, sad"

        Just preset
          | not (canSet preset.access) ->
              H.div "" $ H.text "no can do buddy"

          | otherwise ->
              H.div "border rounded p-2 m-2"
              [ H.strong "" preset.name
              , H.select_
                "form-select"
                -- how with Elmish? aria-label="Default select example"
                { onChange: dispatch
                   <|  PublishDeviceMsg
                   <<< MQTT.mkGenericPublishMsg
                        (setTopic device.name)
                        (fromMaybe "CHECK_YOUR_PROPERTY" preset.property)
                   <<< E.selectSelectedValue
                }
                $ enumValues preset <#> \v -> H.option_ "" { value: v } v
              , H.p "" $ H.text $ fromMaybe "" preset.description
              ]

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

--    list
--      :: forall s. Maybe DeviceState
--      -> s
--      -> CapabilityBase ListProps
--      -> ReactElement
--    list ds _s cap =
--      generic ds cap $ ", item_type: " <> show cap.itemType

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
-- import Control.Alternative (guard)
--
--    listAccess :: Int -> String
--    listAccess a = intercalate ", " $
--      catMaybes
--      [ guard (isPublished a) *> Just "published"
--      , guard (canSet a) *> Just "set"
--      , guard (canGet a) *> Just "get"
--      ]
