module AutomationService.React.ColorWheel
  ( colorWheel
  )
where

import Elmish.Dispatch (EventHandler)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)

type Props = ( onChange :: EventHandler (Object (Object Number)) )

colorWheel :: ImportedReactComponentConstructor Props
colorWheel = createElement' colorWheel_

foreign import colorWheel_ :: ImportedReactComponent
