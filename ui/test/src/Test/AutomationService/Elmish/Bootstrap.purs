--
-- other than the comments around ensureDom_, this is verbatim copied
-- from https://github.com/collegevine/purescript-elmish-testing-library/blob/f867a4c57f2f98fc731aa80506ecae22e57e78e2/src/Elmish/Test/Bootstrap.purs
--
-- I'd prefer to use that module, but I'm not sure how to get around
-- the necessity to install happy-dom/global-registrator and all its
-- dependencies, including node core libraries, and that just seems
-- stupid. But maybe this is worse? I dunno.
--
-- I'll submit a patch if I can figure out what a workable solution
-- that doesn't break the library as-is would be.
--
module Test.AutomationService.Elmish.Bootstrap
  ( testComponent
  , testElement
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Traversable (traverse_)
-- import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Elmish (ComponentDef, ReactElement, construct)
import Elmish.React as React
import Elmish.Test.State (TestState(..))
import Web.DOM.ChildNode (remove)
import Web.DOM.Document (createElement)
import Web.DOM.Element as DOM
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as H
import Web.HTML.Window (document)

-- | Mount the given component to a DOM element, run the given computation in
-- | the context of that element, return the computation's result.
-- |
-- | Example:
-- |
-- |     describe "My component" $
-- |       it "should work" $
-- |         testComponent { init, view, update } do
-- |           find "h1" >> text >>= shouldEqual "Hello"
-- |
testComponent :: ∀ m a msg state. MonadAff m => ComponentDef msg state -> ReaderT TestState m a -> m a
testComponent def go = do
  root <- liftEffect mount
  result <- runReaderT go $ TestState { root, current: root }
  liftEffect $ React.unmount root
  liftEffect $ remove $ DOM.toChildNode root
  pure result
  where
    mount = do
      -- ensureDom_

      doc <- window >>= document
      root <- doc # toDocument # createElement "div"
      doc # body >>= traverse_ \theBody ->
        appendChild (DOM.toNode root) (H.toNode theBody)

      reactEl <- construct def
      React.render reactEl root

      pure root

-- | A convenience version of `testComponent` for "pure" components - i.e.
-- | components that consist only of `view`, no `init` or `update`.
testElement :: ∀ m a. MonadAff m => ReactElement -> ReaderT TestState m a -> m a
testElement element =
  testComponent { init: pure unit, view: \_ _ -> element, update: \_ _ -> pure unit }

-- foreign import ensureDom_ :: Effect Unit
