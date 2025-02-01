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
import AutomationService.DeviceMessage (Message(..))
import AutomationService.DeviceState (DeviceState, DeviceStates, getDeviceState)
import AutomationService.Exposes (SubProps(..), canGet, canSet, enumValues, isOn, isPublished)
import AutomationService.Lighting (ColorSetter(..), getColorSetter, getNumericCap,
                                   getOnOffSwitch, getPreset)
import AutomationService.Logging (debug)
import AutomationService.MQTT as MQTT
import AutomationService.React.Bootstrap as Bootstrap
import AutomationService.React.ColorWheel (colorWheel)
import AutomationService.WebSocket (class WebSocket, sendJson, sendString)
import Color as Color
import Control.Alternative (guard)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (catMaybes, intercalate, sortBy)
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldr)
import Data.List as L
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
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

update
  :: forall ws. WebSocket ws
  => Maybe ws
  -> State
  -> Message
  -> Transition Message State
update ws s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ debug $ "loaded devices: " <> show newDevices
    forkVoid $ do
      liftEffect $ for_ newDevices $ \d -> do
        let
          -- this needs to get passed in from parent state as config, or something
          subscribeMsg =
            MQTT.subscribe (deviceTopic (_.name <<< details $ d)) "HTTP 8080"
          pingStateMsg =
            MQTT.publish (getTopic (_.name <<< details $ d)) $ MQTT.state ""

        debug $ "subscribing with: " <> (stringify $ encodeJson subscribeMsg)
        debug $
          "pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)

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
view { devices, deviceStates } dispatch =
  H.div "all-devices" -- "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.div "device-count" $ H.text $ (show $ L.length $ M.values devices) <> " Devices"
  , H.fragment $
    (\d -> deviceSummary (getDeviceState deviceStates d) d)
    <$>
    sortBy
      (\deviceA deviceB ->
        compare (_.name <<< details $ deviceA) (_.name <<< details $ deviceB)
      )
      devicesA
  ]

  where
    -- This is converted to an Array first because there is no
    -- instance of Elmish.React.ReactChildren (List ReactElement)
    -- in a few places where I want to map over devices to produce
    -- HTML output...
    -- ...maybe there's a better way?
    devicesA :: Array Device
    devicesA = L.toUnfoldable $ M.values devices

    deviceTitle :: DeviceDetails -> ReactElement
    deviceTitle { name } = H.div "card-title" name

    _listAccess :: Int -> String
    _listAccess a = intercalate ", " $
      catMaybes
      [ guard (isPublished a) *> Just "published"
      , guard (canSet a) *> Just "set"
      , guard (canGet a) *> Just "get"
      ]

    deviceSummary :: Maybe DeviceState -> Device -> ReactElement
    deviceSummary mDeviceState device =
      H.div "col" $
      H.div "card mt-2" $
        H.div "card-body text-bg-light p-3" $
          case device of
            ExtendedColorLight deviceDetails ->
              genericOnOffWithDetails mDeviceState deviceDetails
              [ enumSelector "effect" mDeviceState deviceDetails
              , enumSelector "gradient_scene" mDeviceState deviceDetails
              , numberSlider "brightness" mDeviceState deviceDetails
              , numberSlider "color_temp" mDeviceState deviceDetails
              , numberSlider "color_temp_startup" mDeviceState deviceDetails
              , colorSelector mDeviceState deviceDetails
              ]

            ColorTemperatureLight deviceDetails ->
              genericOnOffWithDetails mDeviceState deviceDetails
              [ enumSelector "effect" mDeviceState deviceDetails
              , numberSlider "brightness" mDeviceState deviceDetails
              , numberSlider "color_temp" mDeviceState deviceDetails
              , numberSlider "color_temp_startup" mDeviceState deviceDetails
              ]

            DimmableLight deviceDetails ->
              genericOnOffWithDetails mDeviceState deviceDetails
              [ numberSlider "brightness" mDeviceState deviceDetails ]

            OnOffLight deviceDetails ->
              genericOnOffWithDetails mDeviceState deviceDetails []

            WindowCovering deviceDetails ->
              genericWithDetails mDeviceState deviceDetails [] []

            UnknownDevice deviceDetails ->
              genericWithDetails mDeviceState deviceDetails [] []

    genericOnOffWithDetails
      :: Maybe DeviceState -> DeviceDetails -> Array ReactElement -> ReactElement
    genericOnOffWithDetails mDeviceState deviceDetails featureComponents =
      genericWithDetails
        mDeviceState
        deviceDetails
        [ onOffSwitch mDeviceState deviceDetails ]
        featureComponents

    --
    -- I want something like this, but see comment in
    -- AutomationService.React.Bootstrap.iconToggle
    --
    -- Bootstrap.accordion { defaultActiveKey: 0 }
    -- [
    --   Bootstrap.card {}
    --   [ Bootstrap.cardHeader {} $
    --       H.div "d-flex flex-row"
    --       [ Bootstrap.iconToggle { eventKey: 0 } "bi bi-sliders"
    --       , deviceTitle device
    --       , onOffSwitch mDeviceState deviceDetails
    --       ]
    --   , Bootstrap.accordionCollapse { eventKey: 0 } $
    --       Bootstrap.cardBody {}
    --       [ enumSelector "effect" mDeviceState deviceDetails
    --       , colorSelector mDeviceState deviceDetails
    --       ]
    --   ]
    -- ]
    --

    genericWithDetails
      :: Maybe DeviceState
      -> DeviceDetails
      -> Array ReactElement
      -> Array ReactElement
      -> ReactElement
    genericWithDetails _mDeviceState deviceDetails headerComponents featureComponents =
      Bootstrap.accordion {} $
        Bootstrap.accordionItem { eventKey: 0 }
        [
          H.div_ "card-header d-flex flex-row justify-content-between"
          { style: H.css { minWidth: "11.5em" } } $
          [ deviceTitle deviceDetails ] <> headerComponents
        , Bootstrap.accordionHeader {} "detailed settings"
        , Bootstrap.accordionBody {} $
          [ deviceInfo deviceDetails ] <> featureComponents
        ]

    deviceInfo :: DeviceDetails -> ReactElement
    deviceInfo { id, name, category, manufacturer, model }  =
      let
        mkRow k v = H.tr "" [ H.td "py-1 px-2" (k <> ": "), H.td "py-1 px-2" v ]
      in
       H.div "" $
       [ H.table "m-2" $
           H.tbody ""
           [ mkRow "name" name
           , mkRow "id" id
           , mkRow "category" category
           , mkRow "manufacturer" (fromMaybe "" manufacturer)
           , mkRow "model" (fromMaybe "" model)
           ]
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
            H.div "form-check form-switch"
            [ H.input_
              "form-check-input"
              { type: "checkbox"
              , role: "switch"
              , id: "flexSwitchCheckDefault_" <> device.id
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
              H.empty
            ]

    colorSelector :: Maybe DeviceState -> DeviceDetails -> ReactElement
    colorSelector _mDeviceState device =
      -- let
      --   hsColor = fromMaybe 1 _mDeviceState
      --   xyColor = fromMaybe 1 _mDeviceState
      -- in
       case getColorSetter device of
         Nothing ->
           H.div "" $ H.text "this doesn't support color selection"

         Just (XYSetter _) ->
           H.div "m-1 p-2 border border-secondary-subtle"
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
           H.div "m-2 border border-primary-subtle"
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
    enumSelector presetName _mDeviceState device =
      case getPreset presetName device of
        Nothing -> H.empty

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

    numberSlider :: String -> Maybe DeviceState -> DeviceDetails -> ReactElement
    numberSlider propName mDeviceState device =
      case getNumericCap propName device of
        Just cap@{ subProps: (Numeric numProps) } ->
          let
            idStr = "numericRange_" <> device.id

            getProp :: String -> DeviceState -> Maybe Int
            getProp propName' ds' = case propName' of
              "brightness" -> ds'.brightness
              "color_temp" -> ds'.colorTemp
              "color_temp_startup" -> ds'.colorTempStartup
              _ -> Nothing

            showMaybe alt = show <<< fromMaybe alt

          in
           H.div ""
           [ H.label_ "form-label" { htmlFor: idStr } $
               H.text cap.name

           , H.input_
             "form-range"
             { type: "range"
             , min: showMaybe 0 numProps.valueMin
             , max: showMaybe 255 numProps.valueMax
             , step: showMaybe 1 numProps.valueStep
             , value: showMaybe 100 (getProp propName =<< mDeviceState)
             , id: idStr
             , onChange: dispatch <?| \e -> do
                 _ds <- mDeviceState
                 Just $ PublishDeviceMsg $
                   MQTT.mkGenericPublishMsg
                     (setTopic device.name)
                     propName $
                     E.inputText e
             }
           ]

        Just { subProps: _ } ->
          H.div "" $ H.text unsupported

        Nothing ->
          H.div "" $ H.text unsupported

      where
        unsupported =
          "Capability " <> propName <> " is not supported for this device."
