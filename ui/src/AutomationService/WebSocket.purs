module AutomationService.WebSocket
  ( class WebSocket
  , addWSEventListener
  , connectToWS
  , sendJson
  , sendString
  )
where

import AutomationService.Message as Main
import Data.Argonaut.Core (Json, stringify)
import Data.Either (either)
import Data.Lens ((^?), _Just)
import Data.Maybe (maybe)
import Data.These (These (..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Elmish.Component (Command)
import Foreign (unsafeFromForeign)
import Parsing (runParser)
import Prelude (Unit, (<<<), ($), (<>), (=<<), bind, const, flip, identity, pure)
import URI.Authority (_hosts)
import URI.HierarchicalPart (_authority)
import URI.Host as Host
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.URI (URIOptions, _hierPart)
import URI.URI as URI
import URI.URIRef (Fragment, HierPath, Host, Path, Port, Query, UserInfo)
import Web.DOM.Document as Document
import Web.Event.EventTarget (eventListener)
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.Event.MessageEvent (data_, fromEvent)
import Web.Socket.WebSocket (create, toEventTarget)
import Web.Socket.WebSocket as WS


class WebSocket ws where
  sendString :: ws -> String -> Effect Unit
  sendJson :: ws -> Json -> Effect Unit
  addWSEventListener :: ws -> (String -> Effect Unit) -> Effect Unit

instance WebSocket WS.WebSocket where
  sendString ws s = WS.sendString ws s

  sendJson ws json = WS.sendString ws $ stringify json

  addWSEventListener ws messageHandler = do
    el <- liftEffect $ eventListener $ \evt -> do
      for_ (fromEvent evt) \msgEvt -> do
        let jsonStr = unsafeFromForeign $ data_ msgEvt
        messageHandler jsonStr
    ET.addEventListener onMessage el false (toEventTarget ws)

connectToWS :: Command Aff (Main.Message WS.WebSocket)
connectToWS { dispatch } = do
  wsUrl <- liftEffect getWsUrl
  ws <- liftEffect $ create wsUrl []
  liftEffect $ dispatch (Main.InitWS ws)

-- URI parsing util

getWsUrl :: Effect String
getWsUrl = do
  htmlDocument <- Window.document =<< HTML.window
  uriStr <- Document.documentURI <<< HTMLDocument.toDocument $ htmlDocument
  let
    protocolStr = "ws://"
    localhostStr = "localhost"
    defaultWsUrl = protocolStr <> localhostStr
    parseUriResult = runParser uriStr $ URI.parser options
  -- this is all very tedious
  pure $ flip (either $ const defaultWsUrl) parseUriResult $ \uri ->
    let
      hosts = uri ^? _hierPart <<< _authority <<< _hosts <<< _Just
    in
     flip (maybe defaultWsUrl) hosts $ case _ of
       This hostName -> protocolStr <> Host.print hostName
       -- this would be strange ¯\_(ツ)_/¯
       That port -> defaultWsUrl <> Port.print port
       Both hostName port ->
         protocolStr <> Host.print hostName <> Port.print port

options :: Record (URIOptions UserInfo (HostPortPair Host Port) Path HierPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }
