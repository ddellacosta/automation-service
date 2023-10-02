module AutomationService.Helpers
  ( allElements
  , maybeHtml
  )
  where

import Prelude

import Data.Array (mapMaybe, (..))
import Data.Maybe (Maybe, maybe)
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Enum.Generic (class GenericBoundedEnum, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)

maybeHtml :: forall a. Maybe a -> (a -> ReactElement) -> ReactElement
maybeHtml = flip (maybe H.empty)

--
-- thanks Fyodor
-- https://stackoverflow.com/a/68249604
--
allElements ::
  forall a rep.
  Generic a rep =>
  GenericBoundedEnum rep =>
  GenericTop rep =>
  GenericBottom rep =>
  Array a
allElements = mapMaybe genericToEnum (idxFrom..idxTo)
  where
    idxFrom = genericFromEnum (genericBottom :: a)
    idxTo = genericFromEnum (genericTop :: a)
