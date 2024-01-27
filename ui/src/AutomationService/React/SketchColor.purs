module AutomationService.React.SketchColor
  ( sketchColor
  )
where

import Effect.Uncurried (EffectFn1)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)
import Prelude (Unit)

type Props = ( onChange :: EffectFn1 (Object String) Unit )

sketchColor :: ImportedReactComponentConstructor Props
sketchColor = createElement' sketchColor_

foreign import sketchColor_ :: ImportedReactComponent
