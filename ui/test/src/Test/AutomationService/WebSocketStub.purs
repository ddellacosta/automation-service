module Test.AutomationService.WebSocketStub where

import Effect (Effect)
import Web.Event.EventTarget (EventTarget)

webSocketStub :: Effect EventTarget
webSocketStub = webSocketStub_

foreign import webSocketStub_ :: Effect EventTarget
