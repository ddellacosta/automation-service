module Test.Runner where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($), discard)
import Test.AutomationService.Device as Test.AutomationService.Device
import Test.AutomationService.Exposes as Test.AutomationService.Exposes
import Test.Main as Test.Main
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

spec :: Spec Unit
spec = do
  Test.AutomationService.Device.spec
  Test.AutomationService.Exposes.spec
  Test.Main.spec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
