module Main where

import Prelude

import AutomationService.Device (Capabilities, Device, DeviceId, Devices, canGet, canSet,
                                 decodeDevice, isPublished)
import AutomationService.Helpers (maybeHtml)
import Control.Alternative (guard)
import Data.Argonaut (JsonDecodeError, parseJson, toArray)
import Data.Array (catMaybes, sortBy)
import Data.Either (Either, either)
import Data.Foldable (foldMap, for_, intercalate)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish (Transition, Dispatch, ReactElement, forks, forkVoid, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign (unsafeFromForeign)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket (create, sendString, toEventTarget)


data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String
  | DeviceSelected DeviceId


type State =
  { devices :: Devices
  , selectedDeviceId :: Maybe DeviceId
  }


init :: Transition Message State
init = do
  forks $ \msgSink -> do
    ws <- liftEffect $ create "ws://localhost:8080" []
    delay (Milliseconds 500.0)
    liftEffect $ sendString ws "woah"

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

  pure { devices: M.empty, selectedDeviceId: Nothing }

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
  [ H.h2 "" "Devices"

  , H.select_
      "device-select"
      { onChange: dispatch <| DeviceSelected <<< E.selectSelectedValue } $
      sortBy (\a b -> compare a.name b.name) (L.toUnfoldable $ M.values devices) <#> \d ->
        H.option_ "" { value: d.id } d.name

  , maybe H.empty listDevice $ flip M.lookup devices =<< selectedDeviceId

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
          , maybeHtml capabilities listCapabilities
          ]
        ]
      ]

    listCapabilities :: Capabilities -> ReactElement
    listCapabilities cs =
      H.li "" $ H.div "" $
      [ H.span "display-block" "capabilities: " ]
      <>
      (cs <#> \cap ->
        H.div "" $
             "name: " <> cap.name
          <> ", type: " <> cap.fType
          <> ", access: " <> (listAccess cap.access)
      )

    listAccess :: Int -> String
    listAccess a = intercalate ", " $
      catMaybes
      [ guard (isPublished a) *> Just "published"
      , guard (canSet a) *> Just "set"
      , guard (canGet a) *> Just "get"
      ]


main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }
