module Test.AutomationService.WebSocketStub where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Prelude (Unit)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget)

webSocketStub :: Effect EventTarget
webSocketStub = webSocketStub_

foreign import webSocketStub_ :: Effect EventTarget
