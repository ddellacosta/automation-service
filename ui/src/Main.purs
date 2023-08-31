module Main where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, parseJson, toArray, toNumber, toString)
import Data.Array (filter, sortBy, snoc)
import Data.Foldable (foldl, for_)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either, fromRight)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Elmish (Transition, Dispatch, ReactElement, (<|), forks, forkVoid, transition)
import Elmish.Boot (defaultMain)
import Elmish.Foreign (readForeign, readForeign')
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket (create, sendString, toEventTarget)


-- Nothing happens in our UI so far, so there are no messages
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

-- The UI is just static text, so there is no initial state
type State =
  { devices :: Array Device
  }

type RawDevice =
  { ieeeAddress :: Maybe String
  , friendlyName :: Maybe String
  , type :: String
  , manufacturer :: Maybe String
  , modelId :: Maybe String
  }

type RawDevices = Array String

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
          (json :: Either JsonDecodeError (Array RawDevice)) = do
            jsonRaw <- parseJson $ unsafeFromForeign $ data_ msgEvt
            decodeJson jsonRaw
        msgSink $ case json of
          Right json' -> LoadDevices $
            foldl
              (\rds rd -> case rd.friendlyName of
                  Just name ->
                    snoc rds
                      { id: rd.ieeeAddress
                      , name
                      , category: rd.type
                      , manufacturer: rd.manufacturer
                      , model: rd.modelId
                      }
                  Nothing  -> rds
              )
              [] json'
          Left msg -> LoadDevicesFailed $ show msg

    liftEffect $ addEventListener onMessage el false (toEventTarget ws)

  pure { devices: [] }


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
