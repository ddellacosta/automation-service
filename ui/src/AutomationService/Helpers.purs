module AutomationService.Helpers
  ( maybeHtml
  )
  where

import Prelude

import Data.Maybe (Maybe, maybe)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H

maybeHtml :: forall a. Maybe a -> (a -> ReactElement) -> ReactElement
maybeHtml = flip (maybe H.empty)
