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

import Debug (trace)

import AutomationService.Capability (BinaryProps, Capability(..), CapabilityBase,
                                     CompositeProps, EnumProps, ListProps,
                                     NumericProps, ValueOnOff(..), canGet, canSet,
                                     getBaseCapability, isPublished)
import AutomationService.Components.Slider (slider)
import AutomationService.Device (Capabilities, Device, DeviceId, Devices, deviceTopic,
                                 getTopic, setTopic)
import AutomationService.DeviceState (DeviceState, DeviceStates)
import AutomationService.DeviceViewMessage (Message(..))
import AutomationService.Helpers (maybeHtml)
import AutomationService.MQTT as MQTT
import AutomationService.React.SketchColor (sketchColor)
import AutomationService.WebSocket (class WebSocket, sendString)
import Control.Alternative (guard)
import Data.Argonaut (decodeJson, fromString)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (catMaybes, filter, head, sortBy)
import Data.DateTime.Instant (Instant(..))
import Data.Either (either)
import Data.Foldable (any, foldMap, foldr, intercalate, null)
import Data.Int as Int
import Data.List as L
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Console (debug, info)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|), (<?|))
import Elmish.HTML (css)
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
          subscribeMsg = MQTT.subscribe (deviceTopic d.name) "HTTP 8080" 
          pingStateMsg = MQTT.publish (getTopic d.name) (encodeJson $ MQTT.state "")
        debug $ "subscribing with: " <> (stringify $ encodeJson subscribeMsg)
        debug $ "pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)
        for_ ws $ \ws' -> do
          sendString ws' <<< encodeJson $ subscribeMsg
          sendString ws' <<< encodeJson $ pingStateMsg
    pure $ s { devices = foldMap (\d@{ id } -> M.singleton id d) newDevices }

  LoadDeviceState deviceState -> do
    forkVoid $ liftEffect $ do
      debug $ "loaded DeviceState: " <> show deviceState
    pure $
      s { deviceStates = M.insert deviceState.device.ieeeAddr deviceState s.deviceStates }

  LoadDevicesFailed msg ->
    (forkVoid $ liftEffect $ debug $ "LoadDevicesFailed with msg: " <> msg) *> pure s

  LoadDeviceStateFailed msg ->
    (forkVoid $ liftEffect $ debug $ "LoadDeviceStateFailed with msg: " <> msg) *> pure s

  DeviceSelected deviceId -> do
    forkVoid $ liftEffect $ debug $ "device: " <> deviceId
    pure $ s { selectedDeviceId = Just deviceId }

  NoDeviceSelected -> do
    forkVoid $ liftEffect $ debug $ "unselecting any device"
    pure $ s { selectedDeviceId = Nothing }

  PublishDeviceMsg topic msg -> do
    forkVoid $ liftEffect $ do
      debug $ "Publishing msg '" <> stringify msg <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' <<< encodeJson <<< MQTT.publish topic $ msg
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
      (sortBy (\a b -> compare a.name b.name) devicesA <#> \d ->
        H.option_ "" { value: d.id } d.name
      )

  , maybeHtml (flip M.lookup devices =<< selectedDeviceId) $
      listDevice (flip M.lookup deviceStates =<< selectedDeviceId)

  , H.fragment $
      devicesA <#> \d -> flip (maybe H.empty) d.capabilities $ \caps ->
        if isLighting caps then
          listLightMini (M.lookup d.id deviceStates) d
        else
          H.empty

  ]

  where
    -- This is converted to an Array first because there is no
    -- instance of Elmish.React.ReactChildren (List ReactElement)
    -- in a few places where I want to map over devices to produce
    -- HTML output...
    -- ...maybe there's a better way?
    devicesA :: Array Device
    devicesA = L.toUnfoldable $ M.values devices

    isLighting :: Capabilities -> Boolean
    isLighting = any (\c -> (_.featureType <<< getBaseCapability $ c) == Just "light")

    getStateCap :: Capabilities -> Maybe (CapabilityBase BinaryProps)
    getStateCap caps =
      let
        filterStateCap cap final = case cap of
          BinaryCap cap' ->
            if cap'.property == Just "state" then
              Just cap'
            else
              final
          _ -> final
      in
       foldr filterStateCap Nothing caps

    getBrightnessCap :: Capabilities -> Maybe (CapabilityBase NumericProps)
    getBrightnessCap caps =
      let
        filterBrightnessCap cap final = case cap of
          NumericCap cap' ->
            if cap'.property == Just "brightness" then
              Just cap'
            else
              final
          _ -> final
      in
       foldr filterBrightnessCap Nothing caps

    listLightMini
      :: forall r
       . Maybe DeviceState
      -> { name :: String, capabilities :: Maybe Capabilities | r }
      -> ReactElement
    listLightMini mDeviceState { name, capabilities } =
      let
        valueOn = fromMaybe (ValueOnOffString "ON") $
          _.valueOn <$> (getStateCap =<< capabilities)

        stateStatusClass = case (fromMaybe (ValueOnOffString "OFF") $ _.state =<< mDeviceState) of
          currentState
            | valueOn == currentState ->
              "bg-success text-light fw-bold"  -- "power-button-on"
          _ -> "bg-dark text-secondary text-opacity-75" -- "power-button-off"
      in
        H.div "w-100 border border-1 rounded rounded-2 m-2 p-1 d-flex flex-row justify-content-end"
        [ H.div "" $ H.text name
        , H.div_ ("rounded-3 m-2 p-0 fs-1 " <> stateStatusClass)
          { onClick: dispatch <| \_e ->
             PublishDeviceMsg (setTopic name) <<< encodeJson <<< MQTT.state $ "TOGGLE"
          , role: "button" -- adds pointer behavior
          }
          $ H.i "px-2 bi-power" $ H.empty
        ]


    listDeviceMini
      :: forall r
       . Maybe DeviceState
      -> { name :: String, capabilities :: Maybe Capabilities | r }
      -> ReactElement
    listDeviceMini mDeviceState { name, capabilities }
      | (isLighting <$> capabilities) == Just true =
        H.div_
        "card m-2 p-1"
        {}
        let
          setTopic' = setTopic name
          valueOn = fromMaybe (ValueOnOffString "ON") $
            _.valueOn <$> (getStateCap =<< capabilities)
          stateStatusClass = case (fromMaybe (ValueOnOffString "OFF") $ _.state =<< mDeviceState) of
            currentState
              | valueOn == currentState -> "bg-success text-light fw-bold"  -- "power-button-on"
            _ -> "bg-dark text-secondary text-opacity-75" -- "power-button-off"
        in
         [ H.div "card-body d-flex flex-row justify-content-start align-items-start"
           [ H.div_ "p-2 m-2 me-4 d-flex flex-column justify-content-start align-items-center border border-info"
             { style: css { width: "6rem" } }
             -- on-off button
             [ H.div_ ("rounded-3 m-2 p-2 fs-1 w-75 " <> stateStatusClass)
               { onClick: dispatch <| \_e ->
                  PublishDeviceMsg (setTopic name) <<< encodeJson <<< MQTT.state $ "TOGGLE"
               , role: "button" -- adds pointer behavior
               }
               $ H.i "bi-power" $ H.empty
             , H.span "badge bg-primary fs-7 w-100 text-wrap"
               -- { style: css { overflowWrap: "normal" }}
               name
             ]
             -- brightness slider
           , H.div "slider-wrapper"
               -- { style: css { touchAction: "pan-x" } }
               $ slider
                 { value: Int.toNumber $
                     fromMaybe 50 (_.brightness =<< mDeviceState)
                 , min: fromMaybe 0.0 $
                     capabilities >>= getBrightnessCap >>= _.valueMin <#> Int.toNumber
                 , max: fromMaybe 100.0 $
                     capabilities >>= getBrightnessCap >>= _.valueMax <#> Int.toNumber
                 , onChange: dispatch <?| \e ->
                      let
                        newValue = E.inputText e
                      in Just
                         <<< PublishDeviceMsg setTopic'
                         <<< encodeJson
                         <<< MQTT.genericProp "brightness" $ newValue
                 }
           ]
         ]
      | otherwise =
          H.div "card m-2 p-1" $ H.div "card-body" $ H.text name

    listDevice mDeviceState { id, name, category, model, manufacturer, capabilities } =
      H.div "card mt-2"
      [ H.div "card-body"
        [ H.h4 "" name
        , H.ul ""
          [ H.li "" $ "id: " <> id
          , H.li "" $ "category: " <> category
          , maybeHtml model $ \model' -> H.li "" $ "model: " <> model'
          , maybeHtml manufacturer $ \m -> H.li "" $ "manufacturer: " <> m
          , maybeHtml capabilities $
              H.li "" <<<
                listCapabilities mDeviceState { id, name, category, model, manufacturer }
          ]
        ]
      ]

    listCapabilities
      :: Maybe DeviceState
      -> { id :: String
         , name :: String
         , category :: String
         , model :: Maybe String
         , manufacturer :: Maybe String
         }
      -> Capabilities
      -> ReactElement
    listCapabilities ds s cs =
      H.div "" $
      [ H.span "display-block" "capabilities: " ]
      <>
      (cs <#> case _ of
          BinaryCap cap -> binaryCap ds s cap
          EnumCap cap -> enumCap ds s cap
          NumericCap cap -> numericCap ds s cap
          CompositeCap cap -> compositeCap ds s cap
          ListCap cap -> listCap ds s cap
          GenericCap cap -> genericCap ds cap ""
      )

    binaryCap
      :: forall r. Maybe DeviceState
      -> { name :: String | r}
      -> CapabilityBase BinaryProps
      -> ReactElement
    binaryCap ds s cap
      | not (canSet cap.access) = H.div "" $ H.text "hey"
      | otherwise =
        H.div_ "form-check form-switch" {}
        [ H.input_
          "form-check-input"
          { type: "checkbox"
          , role: "switch"
          , id: "flexSwitchCheckDefault"
          , checked:
              case (fromMaybe (ValueOnOffString "OFF") $ _.state =<< ds) of
                isChecked |
                  ValueOnOffString "ON" == isChecked -> true
                _ -> false

                -- I should test cap.property, but probably in the guard? 
          , onChange: dispatch <| \_e ->
                PublishDeviceMsg (setTopic s.name) <<< encodeJson <<< MQTT.state $ "TOGGLE"
          }
        , H.label_
          "form-check-label"
          { htmlFor: "flexSwitchCheckDefault" } $
          H.text $ "set state for " <> cap.name
        ]

    enumCap
      :: forall r. Maybe DeviceState
      -> { name :: String | r}
      -> CapabilityBase EnumProps
      -> ReactElement
    enumCap ds s cap
      | not (canSet cap.access) = H.div "" $ H.text "hey"
      | otherwise =
        H.div "border rounded p-2 m-2"
        [ H.strong "" cap.name
        , H.select_
            "form-select"
            -- how with Elmish? aria-label="Default select example"
            { onChange: dispatch
                <|  PublishDeviceMsg (setTopic s.name)
                <<< MQTT.genericProp (fromMaybe "CHECK_YOUR_PROPERTY" cap.property)
                <<< E.selectSelectedValue
             }
            $ cap.values <#> \v -> H.option_ "" { value: v } v
        , H.p "" $ H.text $ fromMaybe "" cap.description
        , H.p "" $ H.text $ listAccess cap.access
        , genericCap ds cap ""
        ]

    numericCap
      :: forall r
       . Maybe DeviceState
      -> { id :: DeviceId | r }
      -> CapabilityBase NumericProps
      -> ReactElement
    numericCap ds s cap = case cap.property of
      Just propName ->
        let
          idStr = "numericRange_" <> s.id

          getProp :: String -> DeviceState -> Maybe Int
          getProp propName' ds' = case propName' of
            "brightness" -> ds'.brightness
            "color_temp" -> ds'.colorTemp
            "color_temp_startup" -> ds'.colorTempStartup
            _ -> Nothing

        in
          H.div ""
          [ H.label_ "form-label" { htmlFor: idStr } $
              H.text cap.name
    
          , H.input_
            "form-range"
            { type: "range"
            , min: show $ fromMaybe 0 cap.valueMin
            , max: show $ fromMaybe 255 cap.valueMax
            , step: show $ fromMaybe 1 cap.valueStep
            , value: show $ fromMaybe 100 (getProp propName =<< ds)
            , id: idStr
            , onChange: dispatch <?| \e ->
                 let
                   newValue = E.inputText e
                 in
                   ds >>= \ds' -> Just
                     <<< PublishDeviceMsg (setTopic ds'.device.friendlyName)
                     <<< encodeJson
                     <<< MQTT.genericProp propName $ newValue
            }
  
          , genericCap ds cap ""
          ]

      Nothing -> genericCap ds cap ""

    -- these two are...under-implemented for now
    compositeCap
      :: forall s. Maybe DeviceState
      -> s
      -> CapabilityBase CompositeProps
      -> ReactElement
    compositeCap ds _s cap
      | cap.property == Just "color" =
        H.div ""
        [ sketchColor
          { onChange: dispatch <?| \color -> do
                hex <- O.lookup "hex" color 
                ds' <- ds
                Just
                  <<< PublishDeviceMsg (setTopic ds'.device.friendlyName)
                  <<< encodeJson
                  <<< MQTT.hexColor $ hex 
          }
        , genericCap ds cap $ ", features: " <> show cap.features
        ]
      | otherwise =
        genericCap ds cap $ ", features: " <> show cap.features

    listCap
      :: forall s. Maybe DeviceState
      -> s
      -> CapabilityBase ListProps
      -> ReactElement
    listCap ds _s cap =
      genericCap ds cap $ ", item_type: " <> show cap.itemType

    genericCap
      :: forall r. Maybe DeviceState
      -> CapabilityBase r
      -> String
      -> ReactElement
    genericCap _ds cap capFieldsStr =
      H.div "" $
      [ H.text $ "name: " <> cap.name
        <> ", description: " <> fromMaybe "" cap.description
        <> ", type: " <> cap.capType
        <> ", feature type: " <> fromMaybe "n/a" cap.featureType
        <> ", label: " <> cap.label
        <> ", property: " <> show cap.property
        <> ", access: " <> listAccess cap.access
        <> capFieldsStr
      ]

    listAccess :: Int -> String
    listAccess a = intercalate ", " $
      catMaybes
      [ guard (isPublished a) *> Just "published"
      , guard (canSet a) *> Just "set"
      , guard (canGet a) *> Just "get"
      ]
