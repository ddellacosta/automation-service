module AutomationService.DeviceView
  ( State
  , init
  , view
  , update
  )
where

import Prelude

import AutomationService.Capability (BinaryProps, Capability(..), CapabilityBase,
                                     CompositeProps, EnumProps, ListProps,
                                     NumericProps, canGet, canSet, isPublished)
import AutomationService.Device (Capabilities, DeviceId, Devices, deviceTopic, getTopic, setTopic)
import AutomationService.DeviceState (DeviceState, DeviceStates)
import AutomationService.DeviceViewMessage (Message(..))
import AutomationService.Helpers (maybeHtml)
import AutomationService.WebSocket (class WebSocket, sendString)
import Control.Alternative (guard)
import Data.Argonaut (decodeJson, fromString)
import Data.Array (catMaybes, sortBy)
import Data.Foldable (foldMap, intercalate)
import Data.Either (either)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Console (debug)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H

type State =
  { devices :: Devices
  , deviceStates :: DeviceStates
  , selectedDeviceId :: Maybe DeviceId
  }

init :: Transition Message State
init = pure
  { devices: M.empty
  , deviceStates: M.empty
  , selectedDeviceId: Nothing
  }

update :: forall ws. WebSocket ws => Maybe ws -> State -> Message -> Transition Message State
update ws s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ debug $ "loaded devices: " <> show newDevices
    forkVoid $ do
      liftEffect $ for_ newDevices $ \d -> do
        let
          -- this needs to get passed in from parent state as config, or something
          subscribeMsg =
            "{\"subscribe\": \"HTTP 8080\", \"topic\": \"" <> deviceTopic d.name <> "\"}"
          pingStateMsg =
            "{\"publish\": {\"state\": \"\"}, \"topic\": \"" <> getTopic d.name <> "\"}"
        debug $ "subscribing with: " <> subscribeMsg
        debug $ "pinging to get initial state: " <> pingStateMsg
        for_ ws $ \ws' -> do
          sendString ws' subscribeMsg
          sendString ws' pingStateMsg
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

  PublishDeviceMsg topic msg -> do
    forkVoid $ liftEffect $ do
      debug $ "publish msg '" <> msg <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' ("{\"publish\":" <> msg <> ", \"topic\": \"" <> topic <> "\"}")
    pure s


view :: State -> Dispatch Message -> ReactElement
view { devices, deviceStates, selectedDeviceId } dispatch =
  H.div "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.h3 "" "Devices"

  , H.select_
      "device-select"
      { onChange: dispatch <| DeviceSelected <<< E.selectSelectedValue } $

      -- This is converted to an Array first because there is no
      -- instance of Elmish.React.ReactChildren (List ReactElement)
      -- ...maybe there's a better way?
      sortBy (\a b -> compare a.name b.name) (L.toUnfoldable $ M.values devices) <#> \d ->
        H.option_ "" { value: d.id } d.name

  , maybeHtml (flip M.lookup devices =<< selectedDeviceId) $
      listDevice (flip M.lookup deviceStates =<< selectedDeviceId)

  ]

  where
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
              either
                (const false)
                (\state' -> if cap.valueOn == state' then true else false)
                (decodeJson $ fromString $ fromMaybe "" $ _.state =<< ds)
          , onChange: dispatch
              <|  PublishDeviceMsg (setTopic s.name)
              <<< (\_t ->
                    "{\"" <> fromMaybe "state" cap.property <> "\": \"TOGGLE\"}"
                  )
              <<< E.inputText
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
                <<< (\propVal -> "{\""
                       <> fromMaybe "CHECK_YOUR_PROPERTY" cap.property
                       <> "\": \""
                       <> propVal
                       <> "\"}"
                    )
                <<< E.selectSelectedValue
             }
            $ cap.values <#> \v -> H.option_ "" { value: v } v
        , H.p "" $ H.text $ fromMaybe "" cap.description
        , H.p "" $ H.text $ listAccess cap.access
        , genericCap ds cap ""
        ]

    numericCap
      :: forall s. Maybe DeviceState
      -> s
      -> CapabilityBase NumericProps
      -> ReactElement
    numericCap _ds _s cap =
      H.div ""
      [ H.label_
        "form-label"
        { htmlFor: "numeric-range" } $
        H.text cap.name

      , H.input_
        "form-range"
        { type: "range"
        , min: "0"
        , max: "100"
        , id: "numeric-range"
        }
      ]

    -- these two are...under-implemented for now
    compositeCap
      :: forall s. Maybe DeviceState
      -> s
      -> CapabilityBase CompositeProps
      -> ReactElement
    compositeCap ds _s cap =
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
