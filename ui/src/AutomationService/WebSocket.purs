module AutomationService.WebSocket
  ( class WebSocket
  , addWSEventListener
  , connectToWS
  , sendString
  )
where

import Prelude

import AutomationService.Message as Main
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (debug, info, warn)
import Elmish.Component (Command)
import Web.Event.EventTarget as ET
import Web.Event.EventTarget (EventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.WebSocket as WS
import Web.Socket.WebSocket (create, toEventTarget)

class WebSocket ws where
  sendString :: ws -> String -> Effect Unit
  addWSEventListener :: ws -> EventListener -> Effect Unit

instance WebSocket WS.WebSocket where
  sendString ws s = WS.sendString ws s

  addWSEventListener ws eventListener =
    ET.addEventListener onMessage eventListener false (toEventTarget ws)

connectToWS :: Command Aff (Main.Message WS.WebSocket)
connectToWS msgSink = do
  ws <- liftEffect $ create "ws://localhost:8080" []
  liftEffect $ msgSink (Main.InitWS ws)
