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
  , inputValue
  , launch
  , locator
  , newPage
  , nth
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

-- Navigation
foreign import goto_ :: Page -> String -> Effect (Promise Unit)

goto :: Page -> String -> Aff Unit
goto p url = toAffE (goto_ p url)

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
