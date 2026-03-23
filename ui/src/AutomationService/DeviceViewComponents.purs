module AutomationService.DeviceViewComponents
  ( colorSelector
  , enumSelector
  , numberSlider
  , onOffSwitch
  )
where

import AutomationService.DeviceMessage (Message)
import AutomationService.DeviceState (DeviceState)
import AutomationService.Capabilities (CapabilityDetails, SubProps(..), ValueOnOff, canSet, enumValues, isOn)
import AutomationService.React.ColorWheel (colorWheel)
import Color as Color
import Color (Color)
import Data.Maybe (Maybe(..), fromMaybe)
import Elmish (Dispatch, ReactElement, (<|), (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Foreign.Object as O
import Prelude ((<<<), ($), (<#>), (=<<), (>>=), (<>), bind, not, pure, show)

onOffSwitch
  :: forall r. Dispatch Message
  -> Message
  -> Maybe { state :: Maybe ValueOnOff | r }
  -> CapabilityDetails
  -> ReactElement
onOffSwitch dispatch msg mDeviceState cap =
  H.div "form-check form-switch"
  [ H.input_
    "form-check-input"
    { type: "checkbox"
    , role: "switch"
    , id: "flexSwitchCheckDefault_" -- <> device.id
    , checked:
      case mDeviceState >>= _.state of
        Just onOffValue -> isOn cap onOffValue
        _ -> false
    , onChange: dispatch <| \_e -> msg
    , disabled: not (canSet cap.access)
    }

  , H.label_
    "form-check-label"
    { htmlFor: "flexSwitchCheckDefault_" } $
    H.empty
  ]

-- lol this doesn't even try to read the current state and set it
-- according to the light type, so it can cheat and get away with
-- just sending messages in hsl format
colorSelector
  :: Dispatch Message
  -> (Color -> Message)
  -> Maybe DeviceState
  -> CapabilityDetails
  -> ReactElement
colorSelector dispatch message _mDeviceState _cap =
  H.div "m-1 p-2 border border-secondary-subtle"
  [ colorWheel
    { onChange: dispatch <?| \color -> do
        --
        -- yeah had problems reading rgb/rgba (NaN everywhere)
        -- and hex/hexa (everything was 0.0) so parsing hs
        -- from hsv and using color to generate hex string
        --
        -- thinking I wrote the EffectFn1 sig wrong for
        -- onChange in colorWheel, maybe should try Foreign,
        -- or Json?
        --
        --
        hsv <- O.lookup "hsv" color
        h <- O.lookup "h" hsv
        s <- O.lookup "s" hsv
        -- from what I understand 0.5 is appropriate as a default for lightness in HSL
        pure <<< message $ Color.hsl h s 0.5
    }
  ]

--  HueSatSetter _ ->
--    H.div "m-2 border border-primary-subtle"
--    [ colorWheel
--      { onChange: dispatch <?| \color -> do
--          --
--          -- All of these values are available coming back from
--          -- colorWheel according to a key dump, at least:
--          --
--          -- ["rgb","hsl","hsv","rgba","hsla","hsva","hex","hexa"]
--          --
--          -- hsl and hsv both seem to barf when I try to read the
--          -- 'l' and 'v' values respectively, and hsl for some
--          -- reason doesn't seem to even populate the saturation
--          -- ¯\_(ツ)_/¯
--          --
--          hsv <- O.lookup "hsv" color
--          h <- O.lookup "h" hsv
--          s <- O.lookup "s" hsv
--          -- see above re: 0.5
--          pure <<< message $ Color.hsl h s 0.5
--      }
--    ]

enumSelector
  :: Dispatch Message
  -> (String -> Message)
  -> Maybe DeviceState
  -> CapabilityDetails
  -> ReactElement
enumSelector dispatch message _mDeviceState preset =
  H.div "border rounded p-2 m-2"
  [ H.strong "" preset.name
  , H.select_
    "form-select"
    -- how with Elmish?
    -- aria-label="Default select example"
    { onChange: dispatch <| message <<< E.selectSelectedValue
    , disabled: not (canSet preset.access)
    }
    $ enumValues preset <#> \v -> H.option_ "" { value: v } v
  , H.p "" $ H.text $ fromMaybe "" preset.description
  ]


type DeviceOrGroupId = String

--
-- DeviceOrGroupId is here just as some entropy for generating a
-- distinct ID.
--
numberSlider
  :: Dispatch Message
  -> (String -> Message)
  -> String
  -> Maybe DeviceState
  -> DeviceOrGroupId
  -> CapabilityDetails
  -> ReactElement
numberSlider dispatch message propName mDeviceState deviceOrGroupId cap@{ subProps } =
  case subProps of
    Numeric numProps ->
      let
        idStr = "numericRange_" <> deviceOrGroupId

        getProp :: String -> DeviceState -> Maybe Int
        getProp propName' ds' = case propName' of
          "brightness" -> ds'.brightness
          "color_temp" -> ds'.colorTemp
          "color_temp_startup" -> ds'.colorTempStartup
          _ -> Nothing

        showMaybe alt = show <<< fromMaybe alt

      in
       H.div ""
       [ H.label_ "form-label" { htmlFor: idStr } $
           H.text cap.name

       , H.input_
         "form-range"
         { type: "range"
         , min: showMaybe 0 numProps.valueMin
         , max: showMaybe 255 numProps.valueMax
         , step: showMaybe 1 numProps.valueStep
         , value: showMaybe 100 (getProp propName =<< mDeviceState)
         , id: idStr
         , onChange: dispatch <?| Just <<< message <<< E.inputText
         }
       ]

    _ ->
      H.div "" $ H.text "naw"
