module AutomationService.React.ColorWheel
  ( colorWheel
  )
where

import Effect.Uncurried (EffectFn1)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)
import Prelude (Unit)

type Props = ( onChange :: EffectFn1 (Object String) Unit )

colorWheel :: ImportedReactComponentConstructor Props
colorWheel = createElement' colorWheel_

foreign import colorWheel_ :: ImportedReactComponent
