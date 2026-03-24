module AutomationService.Group
  ( Group
  , GroupDevice
  , GroupScene
  , decodeGroups
  , findGroupDeviceStates
  , groupDevicesOnOffState
  )
where

import AutomationService.Capabilities (ValueOnOff(..), Capability(..), isOn)
import AutomationService.Device (Device, Devices, _capabilities, _deviceDetails, _id)
import AutomationService.DeviceState (DeviceState, DeviceStates)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, toArray)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Array ((:), filter)
import Data.Array as A
import Data.Either (Either(..), isRight)
import Data.Foldable (all, foldMap, foldr)
import Data.Lens ((^.), folded, foldrOf)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Traversable (sequence)
import Prelude ((<<<), (>>>), ($), (#), (<$>), (<#>), (=<<), (<>), (==), bind, pure, flip, identity)

type GroupScene = { id :: Int, name :: String }
type GroupDevice = { id :: String, endpoint :: Int, device :: Device }

type Group =
  { name :: String
  , id :: Int
  , members :: Array GroupDevice
  , scenes :: Array GroupScene
  }

decodeGroups :: Devices -> Json -> Either JsonDecodeError (Array Group)
decodeGroups devices groupsJson = do
  case toArray groupsJson of
    Just gs -> 
      -- REPORT THESE FAILURES! Either isn't really enough for what I want here?
      case (filter isRight $ decodeGroup devices <$> gs) of
        [] -> Left (UnexpectedValue groupsJson)
        ds' -> sequence ds'
    Nothing ->
      Left (UnexpectedValue groupsJson)

decodeGroup :: Devices -> Json -> Either JsonDecodeError Group
decodeGroup devices groupJson = do
  obj <- decodeJson groupJson
  name <- obj .: "friendly_name"
  id <- obj .: "id"
  members <- decodeGroupResource (decodeMember devices) =<< obj .: "members"
  scenes <- decodeGroupResource decodeScene =<< obj .: "scenes"
  if name == "default_bind_group" then
    -- Not exactly what I want here but this felt like the best place
    -- to do this?
    Left (TypeMismatch "default_binding_group")
  else
    Right { name, id, members, scenes }

  where
    decodeGroupResource
      :: forall a. (Json -> Either JsonDecodeError a)
      -> Array Json
      -> Either JsonDecodeError (Array a)
    decodeGroupResource decodeFn grs =
      case (filter isRight $ decodeFn <$> grs) of
        [] -> Right []
        grs' -> sequence grs'

    decodeMember :: Devices -> Json -> Either JsonDecodeError GroupDevice
    decodeMember devices' memberJson = do
      obj <- decodeJson memberJson
      endpoint <- obj .: "endpoint"
      id <- obj .: "ieee_address"
      case M.lookup id devices' of
        Just device ->
          pure { id, endpoint, device }
        Nothing ->
          Left <<< TypeMismatch $ "No device with id " <> id <> " exists"

    decodeScene :: Json -> Either JsonDecodeError GroupScene
    decodeScene sceneJson = do
      obj <- decodeJson sceneJson
      id <- obj .: "id"
      name <- obj .: "name"
      pure { id, name }


-- lookup helpers

findGroupDeviceStates :: Group -> DeviceStates -> Array DeviceState
findGroupDeviceStates group deviceStates =
  let
    groupMemberIDs = Set.fromFoldable $ group.members <#> _.id
  in
   deviceStates
   # M.filterKeys (flip Set.member groupMemberIDs)
   >>> M.values
   >>> A.fromFoldable

--
-- O(n*m*o)
-- where n = group memberDevices, m = Capabilities, and o = DeviceStates
--
groupDevicesOnOffState :: Group -> Array DeviceState -> { state :: Maybe ValueOnOff }
groupDevicesOnOffState group groupMemberDeviceStates =
  --
  -- this all feels pretty tortured, I gotta say
  -- I'm not sure if it's my fault or the fact that there's no
  -- defined contract between groups and devices vis-a-vis zigbee2mqtt
  --
  -- maybe a little of both
  --
  let
    --
    -- Extracts the result of evaluating each OnOff value for all
    -- group member Devices in the context of the DeviceState. This
    -- will naturally only return a non-empty result in the case that
    -- the state matches one the of devices.
    --
    extractOnOffValues :: DeviceState -> Array GroupDevice -> Array Boolean
    extractOnOffValues deviceState members =
      (flip foldMap members) \{ device: memberDevice } ->
        let
          memberDeviceId = memberDevice ^. _deviceDetails <<< _id
        in
         foldrOf
           (_deviceDetails <<< _capabilities <<< folded)
           (\cap onValues' ->
             -- If the device is the same one that we're evaluating
             -- state for, and we get an OnOff capability, evaluate
             -- its state and prepend it to the existing onValues';
             -- otherwise return onValues' as is and keep going.
             case deviceState.device.ieeeAddr == memberDeviceId, cap of
               true, (OnOff cd) ->
                 -- kinda annoying boilerplate. Would not be
                 -- necessary if there were DeviceState types per
                 -- Capability type? Would that more generally
                 -- simplify a lot of DeviceState <-> Capability
                 -- calculations?
                 flip (maybe onValues') deviceState.state \state' ->
                   isOn cd state' : onValues'
               _, _ -> onValues'
           )
           []
           memberDevice

    onOffState =
      foldr
        (\deviceState groupState ->
          let
            onValues = extractOnOffValues deviceState group.members
          in
           case groupState, A.null onValues, all identity onValues of
             --
             -- If groupState has turned false then state does not
             -- change. (False irrevocably converts state to false
             -- until the entire collection of state values is
             -- consumed; this group is off.)
             --
             false, _,     _    -> false
             --
             -- If the existing state is true and the array is not null
             -- and evaluating the state of this current deviceState
             -- value in the context of group memberDevices results in
             -- true, then we carry that state: true value forward.
             --
             true,  false, true -> true
             --
             -- If the existing state is null and there are no
             -- onValues to evaluate, carry that state: true value
             -- forward.
             --
             true,  true,  _    -> true
             --
             -- In any other case set state: false.
             --
             _,     _,     _    -> false
        )
        -- We start true as otherwise this will always fail based on
        -- the foldr logic described above.
        true
        groupMemberDeviceStates

  in
   if onOffState then
     { state: Just (ValueOnOffString "ON") }
    else
     { state: Just (ValueOnOffString "OFF") }
