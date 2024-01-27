module AutomationService.Components.SVG
 ( path_
 )
where

import Elmish.HTML.Internal as I

type OptProps_path =
  ( d :: String
  , fill :: String
  , stroke :: String
  , strokeWidth :: Number
  )

path_ :: I.StyledTagNoContent_ OptProps_path
path_ = I.styledTagNoContent_ "path"
