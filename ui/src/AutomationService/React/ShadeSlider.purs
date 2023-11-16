module AutomationService.React.ShadeSlider
  ( shadeSlider
  )
where

import Effect.Uncurried (EffectFn1)
import Elmish.React (createElement')
import Elmish.React.Import (ImportedReactComponentConstructor, ImportedReactComponent)
import Foreign.Object (Object)
import Prelude (Unit)

type HSVA =
  { h :: Int
  , s :: Int
  , v :: Int
  , a :: Int
  }

type Props =
  ( hsva :: HSVA
  , onChange :: EffectFn1 (Object String) Unit
  )

shadeSlider :: ImportedReactComponentConstructor Props
shadeSlider = createElement' shadeSlider_

foreign import shadeSlider_ :: ImportedReactComponent
