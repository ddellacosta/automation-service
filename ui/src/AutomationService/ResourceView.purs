module AutomationService.ResourceView
  ( State
  , init
  , initState
  , update
  , view
  )
where

import AutomationService.ResourceMessage (Message(..))
import AutomationService.DeviceMessage (Message(..)) as Devices
import AutomationService.Group (Group)
import AutomationService.GroupView (State, init, initState, update, view) as Groups
import AutomationService.DeviceView (DeviceStateUpdateTimers, State, initState, update, view) as Devices
import AutomationService.GroupMessage (Message(..)) as Groups
import AutomationService.WebSocket (class WebSocket)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Elmish (Transition, Dispatch, ReactElement, forkVoid, (<|), (<?|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Prelude (($), (<<<), (<>), (#), (+), bind, pure, show)

import Debug (traceM)

type State ws =
  { deviceState :: Devices.State
  , groupState :: Groups.State
  , websocket :: Maybe ws
  , cnt :: Int
  }

initState
  :: forall ws. WebSocket ws
  => Ref Devices.DeviceStateUpdateTimers
  -> Int
  -> State ws
initState newDsUpdateTimers cnt =
  { deviceState: Devices.initState newDsUpdateTimers
  , groupState: Groups.initState
  , websocket: Nothing
  , cnt: cnt
  }

init
  :: forall ws. WebSocket ws
  => Ref Devices.DeviceStateUpdateTimers
  -> Int
  -> Transition Message (State ws)
init ts = pure <<< initState ts

update
  :: forall ws. WebSocket ws
  => State ws
  -> Message
  -> Transition Message (State ws)
update s@{ deviceState, groupState, websocket } = case _ of
  DeviceMsg deviceMsg -> do
    Devices.update websocket deviceState deviceMsg #
      bimap DeviceMsg (s { deviceState = _ })

  GroupMsg groupMsg -> do
    Groups.update websocket groupState groupMsg #
      bimap GroupMsg (s { groupState = _ })

  UpdateCnt ->
    pure $ s { cnt = s.cnt + 1 }

view
  :: forall ws. WebSocket ws
  => State ws
  -> Dispatch Message
  -> ReactElement
view { cnt, deviceState, groupState } dispatch =
  H.div ""
  [ H.h2 "" $ "COUNT: " <> show cnt
  , Groups.view deviceState groupState $ dispatch <<< GroupMsg
  , Devices.view deviceState $ dispatch <<< DeviceMsg
  ]
