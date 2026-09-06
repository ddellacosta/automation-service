module AutomationService.React.SketchColor
  ( sketchColor
  )
where

import Elmish.Dispatch (EventHandler)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)

type Props = ( onChange :: EventHandler (Object String) )

sketchColor :: ImportedReactComponentConstructor Props
sketchColor = createElement' sketchColor_

foreign import sketchColor_ :: ImportedReactComponent
