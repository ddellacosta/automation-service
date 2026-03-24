module Test.AutomationService.WebSocket where

import AutomationService.WebSocket (getWsUrl')
import Prelude (Unit, ($), discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec =
  describe "WebSocket utilities" $
    it "Constructs the WebSocket URL based on the HTTP URL" $ do

      getWsUrl' "http://foobar.com"
        `shouldEqual`
        { port: ""
        , wsUrl: "ws://foobar.com"
        }

      getWsUrl' "https://foobar.com"
        `shouldEqual`
        { port: ""
        , wsUrl: "wss://foobar.com"
        }
