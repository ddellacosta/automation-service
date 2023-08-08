module Main where

import Prelude

import Elmish (Transition, Dispatch, ReactElement, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Boot (defaultMain)

import Effect (Effect)
import Effect.Console (log)

-- Nothing happens in our UI so far, so there are no messages
data Message

-- The UI is just static text, so there is no initial state
type State = Unit

-- Since there is no state, there is nothing to initialize
init :: Transition Message State
init = pure unit

-- Since there are no messages, the `update` function is also trivial
update :: State -> Message -> Transition Message State
update _ _ = pure unit

view :: State -> Dispatch Message -> ReactElement
view _ _ =
  H.div "container-sm mx-auto mt-5"
  [ H.text "Hello, "
  , H.strong "" "World!"
  ]

main :: Effect Unit
main = defaultMain
  { def: { init, view, update }
  , elementId: "app"
  }
