module Test.Playwright.RouteWebSocket
  ( WebSocketRoute
  , routeWebSocket
  , sendToPage
  , onMessage
  , onClose
  , closeRoute
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Test.Playwright (Page)

-- | Opaque type representing a Playwright WebSocketRoute.
-- | This is the handle you use to send/receive messages
-- | on behalf of the "server" side of an intercepted WebSocket.
foreign import data WebSocketRoute :: Type

-- | Intercept WebSocket connections matching the given URL pattern.
-- |
-- | The callback receives a `WebSocketRoute` for each connection
-- | the page opens. The real server is never contacted.
-- |
-- | Example:
-- |   routeWebSocket page "**" \ws -> do
-- |     sendToPage ws "[{\"devices\": ...}]"
foreign import routeWebSocket_
  :: Page
  -> String
  -> (WebSocketRoute -> Effect Unit)
  -> Effect (Promise Unit)

routeWebSocket :: Page -> String -> (WebSocketRoute -> Effect Unit) -> Aff Unit
routeWebSocket page pattern handler = toAffE (routeWebSocket_ page pattern handler)

-- | Send a message to the page as if it came from the server.
foreign import sendToPage_ :: WebSocketRoute -> String -> Effect Unit

sendToPage :: WebSocketRoute -> String -> Effect Unit
sendToPage = sendToPage_

-- | Register a handler for messages the page sends to the "server".
-- | The handler receives the message string.
foreign import onMessage_ :: WebSocketRoute -> (String -> Effect Unit) -> Effect Unit

onMessage :: WebSocketRoute -> (String -> Effect Unit) -> Effect Unit
onMessage = onMessage_

-- | Register a handler for when the page closes the WebSocket.
-- | The handler receives the close code and reason.
foreign import onClose_
  :: WebSocketRoute
  -> (Int -> String -> Effect Unit)
  -> Effect Unit

onClose :: WebSocketRoute -> (Int -> String -> Effect Unit) -> Effect Unit
onClose = onClose_

-- | Close the server side of the intercepted WebSocket.
foreign import closeRoute_
  :: WebSocketRoute
  -> { code :: Int, reason :: String }
  -> Effect Unit

closeRoute :: WebSocketRoute -> { code :: Int, reason :: String } -> Effect Unit
closeRoute = closeRoute_
