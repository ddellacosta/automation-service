module AutomationService.WebSocket
  ( class WebSocket
  , addWSEventListener
  , connectToWS
  , sendJson
  , sendString
  )
where

import Prelude

import AutomationService.Message as Main
import Data.Argonaut.Core (Json, stringify)
import Data.Either (Either (..))
import Data.Lens ((^?), _Just)
import Data.Maybe (Maybe(..))
import Data.These (These (..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Elmish.Component (Command)
import Web.Event.EventTarget as ET
import Web.Event.EventTarget (EventListener)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.WebSocket as WS
import Web.Socket.WebSocket (create, toEventTarget)

import Web.DOM.Document as Document
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.HTML as HTML
import Parsing (runParser)
import URI.Authority (_hosts)
import URI.HierarchicalPart (_authority)
import URI.Host as Host
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Port as Port
import URI.URI (URIOptions, _hierPart)
import URI.URI as URI
import URI.URIRef (Fragment, HierPath, Host, Path, Port, Query, UserInfo)
import Effect.Console (debug)


class WebSocket ws where
  sendString :: ws -> String -> Effect Unit
  sendJson :: ws -> Json -> Effect Unit
  addWSEventListener :: ws -> EventListener -> Effect Unit

instance WebSocket WS.WebSocket where
  sendString ws s = WS.sendString ws s

  sendJson ws json = WS.sendString ws $ stringify json

  addWSEventListener ws eventListener =
    ET.addEventListener onMessage eventListener false (toEventTarget ws)

connectToWS :: Command Aff (Main.Message WS.WebSocket)
connectToWS { dispatch } = do
  wsUrl <- liftEffect getWsUrl
  ws <- liftEffect $ create wsUrl []
  liftEffect $ dispatch (Main.InitWS ws)

-- URI parsing util

-- wow this was way more involved than I expected
getWsUrl :: Effect String
getWsUrl = do
  htmlDocument <- Window.document =<< HTML.window
  uriStr <- Document.documentURI <<< HTMLDocument.toDocument $ htmlDocument
  let
    protocolStr = "ws://"
    localhostStr = "localhost"
    defaultWsUrl = protocolStr <> localhostStr
  case runParser uriStr $ URI.parser options of
    Left _parseError -> pure defaultWsUrl
    Right uri -> do
      let
        hosts = uri ^? _hierPart <<< _authority <<< _hosts <<< _Just
        hostStr = case hosts of
          Just hosts' -> case hosts' of
            This hostName -> protocolStr <> Host.print hostName
            -- this would be strange ¯\_(ツ)_/¯
            That port -> defaultWsUrl <> Port.print port
            Both hostName port ->
              protocolStr <> Host.print hostName <> Port.print port
          Nothing -> defaultWsUrl
      pure hostStr

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
