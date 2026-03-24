module Test.Runner where

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Prelude (Unit, discard)
import Test.AutomationService.Device as Test.AutomationService.Device
import Test.AutomationService.Capabilities as Test.AutomationService.Capabilities
import Test.AutomationService.Group as Test.AutomationService.Group
import Test.AutomationService.WebSocket as Test.AutomationService.WebSocket
import Test.Main as Test.Main
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

spec :: Spec Unit
spec = do
  Test.AutomationService.Device.spec
  Test.AutomationService.Capabilities.spec
  Test.AutomationService.Group.spec
  Test.AutomationService.WebSocket.spec
  Test.Main.spec

defaultTimeout :: Maybe Milliseconds
defaultTimeout = Just (Milliseconds 50000.0)
-- no timeout
-- defaultTimeout = Nothing

main :: Effect Unit
main = runSpecAndExitProcess'
  { defaultConfig: (defaultConfig { timeout = defaultTimeout })
  , parseCLIOptions: false
  }
  [ consoleReporter ]
  spec
