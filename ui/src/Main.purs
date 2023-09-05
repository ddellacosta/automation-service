module Main where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, parseJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Array (sortBy)
import Data.Foldable (for_)
import Data.Either (Either, either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (info, warn)
import Elmish (Transition, Dispatch, ReactElement, forks, forkVoid)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Foreign (unsafeFromForeign)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket (create, sendString, toEventTarget)


data Message
  = LoadDevices (Array Device)
  | LoadDevicesFailed String

type DeviceId = String

type Device =
  { id :: DeviceId
  , name :: String
  , category :: String
  , manufacturer :: Maybe String
  , model :: Maybe String
  }

decodeDevice :: Json -> Either JsonDecodeError Device
decodeDevice json = do
  obj <- decodeJson json
  id <- obj .: "ieee_address"
  name <- obj .: "friendly_name"
  category <- obj .: "type"
  manufacturer <- obj .:? "manufacturer"
  model <- obj .:? "model_id"
  pure $ { id, name, category, manufacturer, model }

type State =
  { devices :: Array Device
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
        msgSink $
          either (LoadDevicesFailed <<< show) LoadDevices (decode jsonStr)

    liftEffect $ addEventListener onMessage el false (toEventTarget ws)

  pure { devices: [] }

  where
    decode :: String -> Either JsonDecodeError (Array Device)
    decode jsonStr = do
      devicesBlob <- parseJson jsonStr
      traverse decodeDevice $ fromMaybe [] $ toArray devicesBlob


update :: State -> Message -> Transition Message State
update s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ info $ "loaded devices: " <> show newDevices
    pure $ s { devices = newDevices }

  LoadDevicesFailed msg ->
    (forkVoid $ liftEffect $ warn $ "Failed with msg: " <> msg) *> pure s

view :: State -> Dispatch Message -> ReactElement
view { devices } _ =
  H.div "container mx-auto mt-5 d-flex flex-column justify-content-between"
  [ H.h2 "" "Devices"
  , H.select "device-select" $ sortBy (\a b -> compare a.name b.name) devices <#> \d ->
      H.option_ "" { value: d.id } d.name
  ]

--   where
--     listDevice d = H.div "" d.name

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }
