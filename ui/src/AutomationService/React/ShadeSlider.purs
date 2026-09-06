module AutomationService.React.ShadeSlider
  ( shadeSlider
  )
where

import Elmish.Dispatch (EventHandler)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)

type HSVA =
  { h :: Int
  , s :: Int
  , v :: Int
  , a :: Int
  }

type Props =
  ( hsva :: HSVA
  , onChange :: EventHandler (Object String)
  )

shadeSlider :: ImportedReactComponentConstructor Props
shadeSlider = createElement' shadeSlider_

foreign import shadeSlider_ :: ImportedReactComponent
