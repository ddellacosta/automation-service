module Test.Playwright
  ( Browser
  , Locator
  , Page
  , click
  , close
  , closePage
  , evaluate
  , fill
  , goto
  , content
  , screenshot
  , inputValue
  , launch
  , locator
  , newPage
  , nth
  , onConsole
  , onPageError
  , onResponse
  , onRequestFailed
  , pause
  , textContent
  , waitForSelector
  ) where

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude (($), (<<<), (<$>), Unit)


-- Opaque types
foreign import data Browser :: Type
foreign import data Page :: Type
foreign import data Locator :: Type

-- Lifecycle
foreign import launch_ :: { headless :: Boolean } -> Effect (Promise Browser)
foreign import newPage_ :: Browser -> Effect (Promise Page)
foreign import close_ :: Browser -> Effect (Promise Unit)
foreign import closePage_ :: Page -> Effect (Promise Unit)
foreign import pause_ :: Page -> Effect (Promise Unit)
foreign import onConsole_ :: Page -> (String -> Effect Unit) -> Effect Unit
foreign import onPageError_ :: Page -> (String -> Effect Unit) -> Effect Unit
foreign import onResponse_ :: Page -> (String -> Effect Unit) -> Effect Unit
foreign import onRequestFailed_ :: Page -> (String -> Effect Unit) -> Effect Unit

launch :: { headless :: Boolean } -> Aff Browser
launch opts = toAffE (launch_ opts)

newPage :: Browser -> Aff Page
newPage = toAffE <<< newPage_

close :: Browser -> Aff Unit
close = toAffE <<< close_

closePage :: Page -> Aff Unit
closePage = toAffE <<< closePage_

pause :: Page -> Aff Unit
pause = toAffE <<< pause_ 

-- | Register a handler for page console messages; the handler receives
-- "<type>: <text>" (e.g. "error: Uncaught ...")
onConsole :: Page -> (String -> Effect Unit) -> Aff Unit
onConsole page handler = liftEffect $ onConsole_ page handler

-- | Register a handler for uncaught JS errors in the page
onPageError :: Page -> (String -> Effect Unit) -> Aff Unit
onPageError page handler = liftEffect $ onPageError_ page handler

-- | Register a handler logging "<status> <url>" for every page response
onResponse :: Page -> (String -> Effect Unit) -> Aff Unit
onResponse page handler = liftEffect $ onResponse_ page handler

-- | Register a handler for network-level request failures
onRequestFailed :: Page -> (String -> Effect Unit) -> Aff Unit
onRequestFailed page handler = liftEffect $ onRequestFailed_ page handler

-- Navigation
foreign import goto_ :: Page -> String -> Effect (Promise Unit)
foreign import content_ :: Page -> Effect (Promise String)
foreign import screenshot_ :: Page -> Effect (Promise String)

goto :: Page -> String -> Aff Unit
goto p url = toAffE (goto_ p url)

-- | The page's current HTML (document.documentElement.outerHTML), useful
-- for debugging what actually rendered
content :: Page -> Aff String
content p = toAffE (content_ p)

-- | Full-page PNG screenshot, base64-encoded (for e.g. Allure attachments)
screenshot :: Page -> Aff String
screenshot p = toAffE (screenshot_ p)

-- Locators
foreign import locator_ :: Page -> String -> Effect Locator

-- never going to be using this in Effect so making it consistent
-- with everything else
locator :: Page -> String -> Aff Locator
locator p s = liftEffect $ locator_ p s

-- Locator actions
foreign import click_ :: Locator -> Effect (Promise Unit)
foreign import fill_ :: Locator -> String -> Effect (Promise Unit)
foreign import textContent_ :: Locator -> Effect (Promise (Nullable String))
foreign import nth_ :: Locator -> Int -> Effect Locator
foreign import inputValue_ :: Locator -> Effect (Promise String)
foreign import waitForSelector_ :: Page -> String -> Effect (Promise Unit)

click :: Locator -> Aff Unit
click = toAffE <<< click_

fill :: Locator -> String -> Aff Unit
fill l s = toAffE (fill_ l s)

nth :: Locator -> Int -> Aff Locator
nth l n = liftEffect $ nth_ l n

textContent :: Locator -> Aff (Maybe String)
textContent l = toMaybe <$> (toAffE (textContent_ l))

inputValue :: Locator -> Aff String
inputValue = toAffE <<< inputValue_

waitForSelector :: Page -> String -> Aff Unit
waitForSelector p s = toAffE (waitForSelector_ p s)

-- Evaluate JS in page context (for WS stubbing)
foreign import evaluate_ :: Page -> String -> Effect (Promise Unit)

evaluate :: Page -> String -> Aff Unit
evaluate p js = toAffE (evaluate_ p js)
