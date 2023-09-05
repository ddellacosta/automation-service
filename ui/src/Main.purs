module Main where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, parseJson, toArray, toNumber, toString)
import Data.Argonaut.Decode.Combinators ((.:), (.:?))
import Data.Array (filter, length, singleton, sortBy, snoc)
import Data.Foldable (foldMap, for_)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
    liftEffect $ log "hey"

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
      let
        devicesJson = fromMaybe [] (toArray devicesBlob)
      pure $ foldMap (either (const []) singleton) $ decodeDevice <$> devicesJson


update :: State -> Message -> Transition Message State
update s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ liftEffect $ log $ "loaded devices: " <> show newDevices
    pure $ s { devices = newDevices }

  LoadDevicesFailed msg ->
    (forkVoid $ liftEffect $ log $ "Failed with msg: " <> msg) *> pure s

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
