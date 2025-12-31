module Test.Runner where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.AutomationService.Device as Test.AutomationService.Device
import Test.AutomationService.Capabilities as Test.AutomationService.Capabilities
import Test.AutomationService.Group as Test.AutomationService.Group
import Test.AutomationService.Spec (Spec)
import Test.AutomationService.WebSocket as Test.AutomationService.WebSocket
import Test.Main as Test.Main
import Test.Spec.Mocha (runMocha)

spec :: Spec Unit
spec = do
  Test.AutomationService.Device.spec
  Test.AutomationService.Capabilities.spec
  Test.AutomationService.Group.spec
  Test.AutomationService.WebSocket.spec
  Test.Main.spec

main :: Effect Unit
main = runMocha spec
