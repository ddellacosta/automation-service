module Test.Runner where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.AutomationService.Device as Test.AutomationService.Device
import Test.AutomationService.Exposes as Test.AutomationService.Exposes
import Test.AutomationService.Spec (Spec)
import Test.Main as Test.Main
import Test.Spec.Mocha (runMocha)

spec :: Spec Unit
spec = do
  Test.AutomationService.Device.spec
  Test.AutomationService.Exposes.spec
--  Test.Main.spec

main :: Effect Unit
main = runMocha spec
