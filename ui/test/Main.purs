module Test.Main where

import Prelude

import Effect (Effect)

import Effect.Aff (launchAff_)
import Elmish.Test (find, testComponent, text, (>>))
import Elmish.Test.Events (click)
import Main as Main
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

spec :: Spec Unit
spec =
  describe "Home page" $
    it "Can navigate to different pages" $
      testComponent { init: Main.init, view: Main.view, update: Main.update } do
        find "h2[data-test-id=\"main-title\"]" >> text >>= shouldEqual "Home"
        find "li[data-test-id=\"nav-devices\"] a" >> click
        find "h2[data-test-id=\"main-title\"]" >> text >>= shouldEqual "Devices"
