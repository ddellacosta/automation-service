module AutomationService.WebSocket
  ( class WebSocket
  , connectToWS
  , initializeListeners
  )
where

import Prelude

import AutomationService.Device (decodeDevices)
import AutomationService.DeviceView as Device
import AutomationService.Message as Main
import Data.Either (either)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish.Component (Command)
import Foreign (unsafeFromForeign)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket as WS
import Web.Socket.WebSocket (create, toEventTarget)

class WebSocket ws where
  connectToWS :: Command Aff (Main.Message ws)
  initializeListeners :: ws -> Command Aff Device.Message

instance WebSocket WS.WebSocket where
  connectToWS msgSink = do
    ws <- liftEffect $ create "ws://localhost:8080" []
    liftEffect $ msgSink (Main.InitWS ws)

  initializeListeners ws msgSink = do
    el <- liftEffect $ eventListener $ \evt -> do
      for_ (fromEvent evt) \msgEvt -> do
        let
          -- is there a way to do this with Elmish.Foreign that I'm
          -- missing?
          jsonStr = unsafeFromForeign $ data_ msgEvt
        debug jsonStr
        msgSink $
          either
            (Device.LoadDevicesFailed <<< show)
            Device.LoadDevices
            (decodeDevices jsonStr)
    liftEffect $ addEventListener onMessage el false (toEventTarget ws)
