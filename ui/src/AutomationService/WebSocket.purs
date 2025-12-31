module AutomationService.WebSocket
  ( class WebSocket
  , addWSEventListener
  , connectToWS
  , getWsUrl'
  , sendJson
  , sendString
  )
where

import AutomationService.Message as Main
import Data.Argonaut.Core (Json, stringify)
import Data.Either (either)
import Data.Lens ((^?), _Just)
import Data.Maybe (fromMaybe, maybe)
import Data.String.NonEmpty as NonEmpty
import Data.These (These (..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Elmish.Component (Command)
import Foreign (unsafeFromForeign)
import Parsing (runParser)
import Prelude (Unit, (<<<), (>>>), ($), (<$>), (<#>), (<>), (>>=), bind, const, flip, identity, pure, show)
import URI.Authority (_hosts)
import URI.HierarchicalPart (_authority)
import URI.Host as Host
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.Scheme as Scheme
import URI.URI (URIOptions, _hierPart, _scheme)
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
  {port, wsUrl} <- liftEffect getWsUrl
  ws <- liftEffect $ create (wsUrl <> ":" <> port) []
  liftEffect $ dispatch (Main.InitWS port ws)

-- URI parsing util

getWsUrl :: Effect { port :: String, wsUrl :: String }
getWsUrl =
  HTML.window
  >>= Window.document
  >>= HTMLDocument.toDocument
  >>> Document.documentURI
  <#> getWsUrl'

getWsUrl' :: String -> { port :: String, wsUrl :: String }
getWsUrl' uriStr =
  let
    schemeStr = case _ of
      "https" ->
        "wss://"
      _ ->
        "ws://"
    localhostStr = "localhost"
    defaultPortAndWsUrl = { port: "", wsUrl: (schemeStr "http") <> localhostStr }
    parseUriResult = runParser uriStr $ URI.parser options

  -- this is all very tedious
  in
    flip (either $ const defaultPortAndWsUrl) parseUriResult $ \uri ->
      let
        hosts = uri ^? _hierPart <<< _authority <<< _hosts <<< _Just
        scheme = fromMaybe "http" $
          NonEmpty.toString <<< Scheme.toString <$> uri ^? _scheme
      in
       flip (maybe defaultPortAndWsUrl) hosts $ case _ of
         This hostName ->
           { port: ""
           , wsUrl: schemeStr scheme <> Host.print hostName
           }
         -- this would be strange ¯\_(ツ)_/¯
         That port ->
           { port: show <<< Port.toInt $ port
           , wsUrl: schemeStr scheme <> localhostStr
           }
         Both hostName port ->
           { port: show <<< Port.toInt $ port
           , wsUrl: schemeStr scheme <> Host.print hostName
           }

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
