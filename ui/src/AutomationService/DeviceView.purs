module AutomationService.DeviceView
  ( Message(..)
  , State
  , init
  , view
  , update
  )
where

import Prelude

import AutomationService.Capability (BinaryProps, Capability(..), CapabilityBase, CompositeProps,
                                     EnumProps, ListProps, NumericProps, canGet, canSet,
                                     isPublished)
import AutomationService.Device (Capabilities, Device, DeviceId, Devices, decodeDevice)
import AutomationService.Helpers (maybeHtml)
import Control.Alternative (guard)
import Data.Argonaut (JsonDecodeError, parseJson, toArray)
import Data.Array (catMaybes, sortBy)
import Data.Either (Either, either)
import Data.Foldable (foldMap, for_, intercalate)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|))
import Elmish.Component (Command)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign (unsafeFromForeign)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket (WebSocket, toEventTarget)


data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | DeviceSelected DeviceId

type State =
  { devices          :: Devices
  , selectedDeviceId :: Maybe DeviceId
  }

init :: WebSocket -> Command Aff Message
init ws msgSink = do
  el <- liftEffect $ eventListener $ \evt -> do
    for_ (fromEvent evt) \msgEvt -> do
      let
        -- is there a way to do this with Elmish.Foreign that I'm
        -- missing?
        jsonStr = unsafeFromForeign $ data_ msgEvt
      debug jsonStr
      msgSink $
        either (LoadDevicesFailed <<< show) LoadDevices (decode jsonStr)

  liftEffect $ addEventListener onMessage el false (toEventTarget ws)

  where
    decode :: String -> Either JsonDecodeError (Array Device)
    decode jsonStr = do
      devicesBlob <- parseJson jsonStr
      traverse decodeDevice $ fromMaybe [] $ toArray devicesBlob


update :: State -> Message -> Transition Message State
update s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ info $ "loaded devices: " <> show newDevices
    pure $ s { devices = foldMap (\d@{ id } -> M.singleton id d) newDevices }

  LoadDevicesFailed msg ->
    (forkVoid $ liftEffect $ warn $ "Failed with msg: " <> msg) *> pure s

  DeviceSelected deviceId -> do
    forkVoid $ liftEffect $ info $ "device: " <> deviceId
    pure $ s { selectedDeviceId = Just deviceId }


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
          , maybeHtml capabilities $ H.li "" <<< listCapabilities
          ]
        ]
      ]

    listCapabilities :: Capabilities -> ReactElement
    listCapabilities cs =
      H.div "" $
      [ H.span "display-block" "capabilities: " ]
      <>
      (cs <#> case _ of
          BinaryCap cap -> binaryCap cap
          EnumCap cap -> enumCap cap
          NumericCap cap -> numericCap cap
          CompositeCap cap -> compositeCap cap
          ListCap cap -> listCap cap
          GenericCap cap -> genericCap cap ""
      )

    binaryCap :: CapabilityBase BinaryProps -> ReactElement
    binaryCap cap =
      genericCap cap $
           ", value_on: " <> (show cap.valueOn)
        <> ", value_off: " <> (show cap.valueOff)
        <> ", value_toggle: " <> (show cap.valueToggle)

    enumCap :: CapabilityBase EnumProps -> ReactElement
    enumCap cap =
      genericCap cap $ ", values: " <> (show cap.values)

    numericCap :: CapabilityBase NumericProps -> ReactElement
    numericCap cap =
      genericCap cap $
           ", value_max: " <> (show cap.valueMax)
        <> ", value_min: " <> (show cap.valueMin)
        <> ", value_step: " <> (show cap.valueStep)
        <> ", unit: " <> (show cap.unit)

    compositeCap :: CapabilityBase CompositeProps -> ReactElement
    compositeCap cap =
      genericCap cap $ ", features: " <> (show cap.features)

    listCap :: CapabilityBase ListProps -> ReactElement
    listCap cap =
      genericCap cap $ ", item_type: " <> (show cap.itemType)

    genericCap :: forall r. CapabilityBase r -> String -> ReactElement
    genericCap cap capFieldsStr =
      H.div "" $
      [ H.text $ "name: " <> cap.name
        <> ", description: " <> (fromMaybe "" cap.description)
        <> ", type: " <> cap.capType
        <> ", feature type: " <> (fromMaybe "n/a" cap.featureType)
        <> ", label: " <> cap.label
        <> ", property: " <> (show cap.property)
        <> ", access: " <> (listAccess cap.access)
        <> capFieldsStr
      ]

    listAccess :: Int -> String
    listAccess a = intercalate ", " $
      catMaybes
      [ guard (isPublished a) *> Just "published"
      , guard (canSet a) *> Just "set"
      , guard (canGet a) *> Just "get"
      ]
