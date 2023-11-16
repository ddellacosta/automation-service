module AutomationService.Components.Slider
 ( slider
 )
where

import AutomationService.Components.SVG (path_)
import Data.Number (round)
import Data.Number.Format (toString)
import Data.Unit (Unit)
import Effect.Uncurried (EffectFn1)
import Elmish (ReactElement)
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Styled as H
import Prelude (($), (<>), (*), (-), (/))

slider
  :: forall r
   . { value :: Number
     , min :: Number
     , max :: Number
     , onChange :: EffectFn1 InputChangeEvent Unit
     | r
     }
  -> ReactElement
slider s@{ onChange } =
  H.div "slider"
  [
    H.input_
    ""
    { type: "range"
    , value
    , min
    , max
    , onChange
    }
    , H.svg_
      ""
      { width: "100%"
      , height: "100%"
      , viewBox: "0 0 1 1"
      , preserveAspectRatio: "none"
      } $
        path_
        ""
        { d: "M 0.5 1L0.5 " <> toString (1.0 - roundedProgress)
        , stroke: "rgba(255,255,255,0.6)"
        }
  ]

  where
    min = toString s.min
    max = toString s.max
    value = toString s.value

    progress = (s.value - s.min) / (s.max - s.min)

    roundedProgress = round(progress * 100.0) / 100.0
