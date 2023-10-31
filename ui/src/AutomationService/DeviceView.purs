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
                                     NumericProps, canGet, canSet, isPublished,
                                     serializeValueOnOff)
import AutomationService.Device (Capabilities, DeviceId, Devices)
import AutomationService.DeviceViewMessage (Message(..))
import AutomationService.Helpers (maybeHtml)
import AutomationService.WebSocket (class WebSocket, sendString)
import Control.Alternative (guard)
import Data.Array (catMaybes, sortBy)
import Data.Foldable (foldMap, intercalate)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H

type State =
  { devices :: Devices
  , selectedDeviceId :: Maybe DeviceId
  }

init :: Transition Message State
init = pure
  { devices: M.empty
  , selectedDeviceId: Nothing
  }

update :: forall ws. WebSocket ws => Maybe ws -> State -> Message -> Transition Message State
update ws s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ info $ "loaded devices: " <> show newDevices
    pure $ s { devices = foldMap (\d@{ id } -> M.singleton id d) newDevices }

  LoadDevicesFailed msg ->
    (forkVoid $ liftEffect $ warn $ "Failed with msg: " <> msg) *> pure s

  DeviceSelected deviceId -> do
    forkVoid $ liftEffect $ info $ "device: " <> deviceId
    pure $ s { selectedDeviceId = Just deviceId }

  PublishDeviceMsg topic msg -> do
    forkVoid $ liftEffect $ do
      debug $ "publish msg '" <> msg <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' ("{\"publish\":" <> msg <> ", \"topic\": \"" <> topic <> "\"}")
    pure s


view :: State -> Dispatch Message -> ReactElement
view { devices, selectedDeviceId } dispatch =
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

  , maybeHtml (flip M.lookup devices =<< selectedDeviceId) listDevice

  ]

  where
    listDevice { id, name, category, model, manufacturer, capabilities } =
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
                listCapabilities { id, name, category, model, manufacturer }
          ]
        ]
      ]

    setTopic name = "zigbee2mqtt/" <> name <> "/set"
    getTopic name = "zigbee2mqtt/" <> name <> "/get"

    listCapabilities
      :: { id :: String
         , name :: String
         , category :: String
         , model :: Maybe String
         , manufacturer :: Maybe String
         }
      -> Capabilities
      -> ReactElement
    listCapabilities s cs =
      H.div "" $
      [ H.span "display-block" "capabilities: " ]
      <>
      (cs <#> case _ of
          BinaryCap cap -> binaryCap s cap
          EnumCap cap -> enumCap s cap
          NumericCap cap -> numericCap s cap
          CompositeCap cap -> compositeCap s cap
          ListCap cap -> listCap s cap
          GenericCap cap -> genericCap cap ""
      )

    binaryCap :: forall r. { name :: String | r} -> CapabilityBase BinaryProps -> ReactElement
    binaryCap s cap =
      H.div_ "form-check form-switch" {}
      [ H.input_
        "form-check-input"
        { type: "checkbox"
        , role: "switch"
        , id: "flexSwitchCheckDefault"
        , value: serializeValueOnOff cap.valueOn
        -- this will become more sophisticated once this takes into
        -- account the currently set value, and whether or not any
        -- capability has the 'set' permission--this is a placeholder
        -- spike
        , onChange: dispatch
            <|  PublishDeviceMsg (setTopic s.name)
            <<< (\_t -> "{\"" <> fromMaybe "state" cap.property <> "\": \"" <> "TOGGLE" <> "\"}")
            <<< E.inputText
        }
      , H.label_
        "form-check-label"
        { htmlFor: "flexSwitchCheckDefault" } $
        H.text $ "set state for " <> cap.name
      ]

    enumCap :: forall r. { name :: String | r} -> CapabilityBase EnumProps -> ReactElement
    enumCap s cap =
      H.div "border rounded p-2 m-2"
      [ H.strong "" cap.name
      , H.select_
          "form-select"
          -- not with Elmish? aria-label="Default select example"
          { onChange: dispatch
              <|  PublishDeviceMsg (setTopic s.name)
              <<< (\s -> "{\""
                     <> fromMaybe "CHECK_YOUR_PROPERTY" cap.property
                     <> "\": \""
                     <> s <> "\"}"
                  )
              <<< E.selectSelectedValue
           }
          $ cap.values <#> \v -> H.option_ "" { value: v } v
      , H.p "" $ H.text $ fromMaybe "" cap.description
      , H.p "" $ H.text $ listAccess cap.access
      , genericCap cap ""
      ]

    numericCap :: forall s. s -> CapabilityBase NumericProps -> ReactElement
    numericCap _s cap =
      H.div ""
      [ H.label_
        "form-label"
        { htmlFor: "customRange2" } $
        H.text cap.name

      , H.input_
        "form-range"
        { type: "range"
        , min: "0"
        , max: "100"
        , id: "customRange2"
        }
      ]

    compositeCap :: forall s. s -> CapabilityBase CompositeProps -> ReactElement
    compositeCap _s cap =
      genericCap cap $ ", features: " <> show cap.features

    listCap :: forall s. s -> CapabilityBase ListProps -> ReactElement
    listCap _s cap =
      genericCap cap $ ", item_type: " <> show cap.itemType

    genericCap :: forall r. CapabilityBase r -> String -> ReactElement
    genericCap cap capFieldsStr =
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
