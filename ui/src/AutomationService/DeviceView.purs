module AutomationService.DeviceView
  ( DeviceStateUpdateTimers
  , State
  , init
  , initState
  , view
  , update
  )
where

import AutomationService.Device as Device
import AutomationService.Device (Device(..), DeviceDetails, DeviceId, Devices, _deviceDetails, _capabilities, _id, _name, details, deviceTopic, getTopic, setTopic)
import AutomationService.DeviceMessage (Message(..))
import AutomationService.DeviceState (DeviceState, DeviceStates, getDeviceState)
import AutomationService.DeviceViewComponents as Components
import AutomationService.Capabilities (Capability(..), CapabilityDetails, SubProps(..), ValueOnOff(..), canGet, canSet, capabilityDetails, isOn, isPublished)
import AutomationService.Group (Group, GroupDevice)
import AutomationService.Group as Group
import AutomationService.Logging (debug, warn)
import AutomationService.MQTT as MQTT
import AutomationService.React.Bootstrap as Bootstrap
import AutomationService.WebSocket (class WebSocket, sendJson, sendString)
import Color as Color
import Control.Alternative (guard)
import Data.Argonaut (Json, jsonNull)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array as Array
import Data.Array ((:), catMaybes, head, intercalate, sort)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Foldable (all, foldMap, foldr)
import Data.Lens ((^.), folded, foldrOf)
import Data.List as L
import Data.Map as M
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (class Ord)
import Data.Set as Set
import Data.Set (Set)
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Elmish (Transition, Dispatch, ReactElement, forkVoid)
import Elmish.HTML (_data)
import Elmish.HTML.Styled as H
import Prelude (($), (#), (<<<), (>>>), (<$>), (<#>), (*>), (<>), (==), compare, discard, flip, identity, pure, show)


data Resource
  = DeviceResource Device
  | GroupResource Group

derive instance Eq Resource

instance Ord Resource where
  compare (DeviceResource d1) (DeviceResource d2) =
    compare (d1 ^. _deviceDetails <<< _name) (d2 ^. _deviceDetails <<< _name)
  compare (GroupResource g1) (GroupResource g2) = compare g1.name g2.name
  compare (DeviceResource d1) (GroupResource g2) = compare (d1 ^. _deviceDetails <<< _name) g2.name
  compare (GroupResource g1) (DeviceResource d2) = compare g1.name (d2 ^. _deviceDetails <<< _name)

--


type DeviceStateUpdateTimers = Map DeviceId Instant

type State =
  { devices :: Devices
  , deviceStates :: DeviceStates
  , selectedDeviceId :: Maybe DeviceId
  , deviceStateUpdateTimers :: Ref DeviceStateUpdateTimers
  , groups :: Array Group
  , groupsJson :: Json
  }

initState :: Ref DeviceStateUpdateTimers -> State
initState dsUpdateTimers =
  { devices: M.empty
  , deviceStates: M.empty
  , selectedDeviceId: Nothing
  , deviceStateUpdateTimers: dsUpdateTimers
  , groups: []
  , groupsJson: jsonNull
  }

init :: Ref DeviceStateUpdateTimers -> Transition Message State
init = pure <<< initState

update
  :: forall ws. WebSocket ws
  => Maybe ws
  -> String
  -> State
  -> Message
  -> Transition Message State
update ws wsPort s = case _ of
  LoadDevices newDevices -> do
    forkVoid $ do
      liftEffect $ debug $ "loaded devices: " <> show newDevices
      liftEffect $ for_ newDevices $ \d -> do
        let
          -- this needs to get passed in from parent state as config, or something
          subscribeMsg =
            MQTT.subscribe (deviceTopic (_.name <<< details $ d)) $ "HTTP " <> wsPort
          pingStateMsg =
            MQTT.publish (getTopic (_.name <<< details $ d)) $ MQTT.state ""

        debug $ "subscribing with: " <> (stringify $ encodeJson subscribeMsg)
        debug $
          "pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)

        for_ ws $ \ws' -> do
          sendJson ws' <<< encodeJson $ subscribeMsg
          sendJson ws' <<< encodeJson $ pingStateMsg
    pure $
      s { devices =
             foldr
               (\d ds -> M.insert (_.id <<< details $ d) d ds)
               M.empty
               newDevices
        }

  -- Currently unimplemented on the WS end
  LoadDeviceState deviceState -> do
    forkVoid $ liftEffect $ do
      debug $ "loaded DeviceState: " <> show deviceState
    pure $
      s { deviceStates =
             M.insert deviceState.device.ieeeAddr deviceState s.deviceStates
        }

  LoadDevicesFailed _msg ->
    -- (forkVoid $ liftEffect $ debug $ "LoadDevicesFailed with msg: " <> msg) *> pure s
    pure s

  LoadDeviceStateFailed _msg ->
    -- (forkVoid $ liftEffect $ debug $ "LoadDeviceStateFailed with msg: " <> msg) *> pure s
    pure s

  DeviceSelected deviceId -> do
    forkVoid $ liftEffect $ debug $ "device: " <> deviceId
    pure $ s { selectedDeviceId = Just deviceId }

  NoDeviceSelected -> do
    forkVoid $ liftEffect $ debug $ "unselecting any device"
    pure $ s { selectedDeviceId = Nothing }

  PublishDeviceMsg msg -> do
    forkVoid $ liftEffect $ do
      debug $ "Publishing with '" <> msg  -- <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' msg
    pure s

  LoadGroups groupsJson -> loadGroups s groupsJson

  ReLoadGroups -> loadGroups s s.groupsJson

  LoadGroupsFailed _msg -> do
    -- forkVoid $
    --   liftEffect $ debug $ "LoadGroupsFailed with msg: " <> msg) *> pure s
    -- _ <- traceM msg
    pure s

  PublishGroupMsg msg -> do
    forkVoid $ liftEffect $ do
      debug $ "Publishing with '" <> msg  -- <> "' to topic: " <> topic
      for_ ws $ \ws' ->
        sendString ws' msg 
    pure s

  where
    loadGroups :: State -> Json -> Transition Message State
    loadGroups state groupsJson =
      case Group.decodeGroups state.devices groupsJson of
        Left groupsDecodingFailure -> do
          forkVoid $ do
            liftEffect $ warn $ "loading groups failed: " <> show groupsDecodingFailure
          pure state

        Right newGroups -> do
          forkVoid $ do
            liftEffect $ warn $ "loaded groups: " <> show newGroups
            liftEffect $ for_ newGroups $ \g -> do
              let
                -- not sure this is worthwhile? It only seems to send
                -- to us when we set group state
                subscribeMsg =
                  MQTT.subscribe (deviceTopic g.name) $ "HTTP " <> wsPort
                pingStateMsg =
                  MQTT.publish (getTopic g.name) $ MQTT.state ""

              warn $
                g.name <> " - Groups: subscribing with: " <> (stringify $ encodeJson subscribeMsg)
              warn $
                g.name <> " - Groups: pinging to get initial state: " <> (stringify $ encodeJson pingStateMsg)

              for_ ws $ \ws' -> do
                sendJson ws' <<< encodeJson $ subscribeMsg
                sendJson ws' <<< encodeJson $ pingStateMsg

          pure state { groupsJson = groupsJson, groups = newGroups }


view :: State -> Dispatch Message -> ReactElement
view state@{ devices, deviceStates, groups } dispatch =
  H.div_ "all-devices" -- "container mx-auto mt-5 d-flex flex-column justify-content-between"
  { _data: _data { "test-id": "all-devices" } }
  [ H.div "device-count" $ H.text $ (show $ L.length $ M.values devices) <> " Devices"
  , H.div "d-flex flex-row justify-content-between flex-wrap" $
      sort resources <#>
        \resource -> case resource of
          DeviceResource d -> deviceSummary (getDeviceState deviceStates d) d
          GroupResource g -> groupSummary g state dispatch
  ]

  where
    -- This is converted to an Array first because there is no
    -- instance of Elmish.React.ReactChildren (List ReactElement)
    -- in a few places where I want to map over devices to produce
    -- HTML output...
    -- ...maybe there's a better way?
    devicesA :: Array Device
    devicesA = L.toUnfoldable $ M.values devices

    resources :: Array Resource
    resources = (DeviceResource <$> devicesA) <> (GroupResource <$> groups)

    deviceTitle :: DeviceDetails -> ReactElement
    deviceTitle { name } = H.div "card-title" name

    _listAccess :: Int -> String
    _listAccess a = intercalate ", " $
      catMaybes
      [ guard (isPublished a) *> Just "published"
      , guard (canSet a) *> Just "set"
      , guard (canGet a) *> Just "get"
      ]

    deviceSummary :: Maybe DeviceState -> Device -> ReactElement
    deviceSummary mDeviceState device =
      H.div "device card mt-2 mx-2 resource-card"  $
        H.div "card-body text-bg-light p-2" $
          case device of

            ExtendedColorLight deviceDetails ->
              genericOnOffWithDetails
                mDeviceState
                (deviceTitle deviceDetails)
                (deviceInfo deviceDetails)
                (onOffSwitch mDeviceState deviceDetails)
                [ enumSelector "effect" mDeviceState deviceDetails
                , enumSelector "gradient_scene" mDeviceState deviceDetails
                , numberSlider "brightness" mDeviceState deviceDetails
                , numberSlider "color_temp" mDeviceState deviceDetails
                , numberSlider "color_temp_startup" mDeviceState deviceDetails
                , colorSelector mDeviceState deviceDetails
                ]

            ColorTemperatureLight deviceDetails ->
              genericOnOffWithDetails
                mDeviceState
                (deviceTitle deviceDetails)
                (deviceInfo deviceDetails)
                (onOffSwitch mDeviceState deviceDetails)
                [ enumSelector "effect" mDeviceState deviceDetails
                , numberSlider "brightness" mDeviceState deviceDetails
                , numberSlider "color_temp" mDeviceState deviceDetails
                , numberSlider "color_temp_startup" mDeviceState deviceDetails
                ]

            DimmableLight deviceDetails ->
              genericOnOffWithDetails
                mDeviceState
                (deviceTitle deviceDetails)
                (deviceInfo deviceDetails)
                (onOffSwitch mDeviceState deviceDetails)
                [ numberSlider "brightness" mDeviceState deviceDetails ]

            OnOffLight deviceDetails ->
              genericOnOffWithDetails
                mDeviceState
                (onOffSwitch mDeviceState deviceDetails)
                (deviceTitle deviceDetails)
                (deviceInfo deviceDetails)
                []

            GenericSwitch deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            ContactSensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            LightSensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            OccupancySensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            TemperatureSensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            HumiditySensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            AirQualitySensor deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            WindowCovering deviceDetails ->
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

            UnknownDevice deviceDetails ->
              H.div "border border-danger" $
              genericWithDetails mDeviceState (deviceTitle deviceDetails) (deviceInfo deviceDetails) [] []

    deviceInfo :: DeviceDetails -> ReactElement
    deviceInfo { id, name, category, manufacturer, model }  =
      let
        mkRow k v = H.tr "" [ H.td "py-1 px-2" (k <> ": "), H.td "py-1 px-2" v ]
      in
       H.div "" $
       [ H.table "m-2" $
           H.tbody ""
           [ mkRow "name" name
           , mkRow "id" id
           , mkRow "category" category
           , mkRow "manufacturer" (fromMaybe "" manufacturer)
           , mkRow "model" (fromMaybe "" model)
           ]
       ]

    onOffSwitch :: Maybe DeviceState -> DeviceDetails -> ReactElement
    onOffSwitch mDeviceState device@{ capabilities } =
      let
        onOffCapabilities = (flip NonEmpty.filter) capabilities \cap -> case cap of
          OnOff _ -> true
          _ -> false

      in
       case head onOffCapabilities <#> capabilityDetails of
         Nothing ->
           H.div "" $ H.text "Not a light?"

         Just cap ->
           Components.onOffSwitch
             dispatch
             (PublishDeviceMsg $ MQTT.mkPublishMsg (setTopic device.name) $ MQTT.state "TOGGLE")
             mDeviceState
             cap

    colorSelector :: Maybe DeviceState -> DeviceDetails -> ReactElement
    colorSelector mDeviceState device@{ capabilities } =
      let
        topic = setTopic device.name
        message color =
          PublishDeviceMsg $
            MQTT.mkPublishMsg topic $
              MQTT.hexColor $ Color.toHexString color
        colorCapabilityDetails = flip NonEmpty.filter capabilities \cap -> case cap of
          ColorXY _ -> true
          ColorHue _ -> true
          ColorHex _ -> true
          _ -> false

      in
       case head colorCapabilityDetails <#> capabilityDetails of
         Nothing ->
           H.div "" $ H.text "this doesn't support color selection"

         Just setter ->
           Components.colorSelector dispatch message mDeviceState setter

    enumSelector :: String -> Maybe DeviceState -> DeviceDetails -> ReactElement
    enumSelector presetName mDeviceState device@{ capabilities } =
      let
        isPreset :: CapabilityDetails -> Boolean
        isPreset { name, subProps } =
          case subProps of
            Enum _props -> name == presetName
            _ -> false

        presetCapabilities = NonEmpty.filter (isPreset <<< capabilityDetails) capabilities

      in
       case head presetCapabilities <#> capabilityDetails of
         Nothing -> H.empty

         Just preset ->
           Components.enumSelector
             dispatch
             (PublishDeviceMsg
              <<< MQTT.mkGenericPublishMsg
                (setTopic device.name)
                (fromMaybe "CHECK_YOUR_PROPERTY" preset.property)
               )
             mDeviceState
             preset

    numberSlider :: String -> Maybe DeviceState -> DeviceDetails -> ReactElement
    numberSlider propName mDeviceState device@{ capabilities } =
      case getNumericCap propName <#> capabilityDetails of
        Just cap ->
          Components.numberSlider
            dispatch
            (PublishDeviceMsg <<< MQTT.mkGenericPublishMsg (setTopic device.name) propName)
            propName
            mDeviceState
            cap

        Nothing ->
          H.div "" $ H.text unsupported

      where
        unsupported =
          "Capability " <> propName <> " is not supported for this device."

        getNumericCap :: String -> Maybe Capability
        getNumericCap capName =
          head $
            NonEmpty.filter
              (capabilityDetails >>>
               case _ of
                 { name, subProps: Numeric _numProps } ->
                   capName == name

                 _ ->
                   false
              )
              capabilities


-- generic helpers for both devices and groups

genericOnOffWithDetails
  :: Maybe DeviceState -> ReactElement -> ReactElement -> ReactElement -> Array ReactElement -> ReactElement
genericOnOffWithDetails mDeviceState titleElement summaryElement onOffSwitch' featureComponents =
  genericWithDetails
    mDeviceState
    titleElement
    summaryElement
    [ onOffSwitch' ]
    featureComponents

--
-- I want something like this, but see comment in
-- AutomationService.React.Bootstrap.iconToggle
--
-- Bootstrap.accordion { defaultActiveKey: 0 }
-- [
--   Bootstrap.card {}
--   [ Bootstrap.cardHeader {} $
--       H.div "d-flex flex-row"
--       [ Bootstrap.iconToggle { eventKey: 0 } "bi bi-sliders"
--       , deviceTitle device
--       , onOffSwitch mDeviceState deviceDetails
--       ]
--   , Bootstrap.accordionCollapse { eventKey: 0 } $
--       Bootstrap.cardBody {}
--       [ enumSelector "effect" mDeviceState deviceDetails
--       , colorSelector mDeviceState deviceDetails
--       ]
--   ]
-- ]
--

genericWithDetails
  :: Maybe DeviceState
  -> ReactElement
  -> ReactElement
  -> Array ReactElement
  -> Array ReactElement
  -> ReactElement
genericWithDetails _mDeviceState titleElement summaryElement headerComponents featureComponents =
  Bootstrap.accordion {} $
    Bootstrap.accordionItem { eventKey: 0 }
    [
      H.div_
        "card-header d-flex flex-row justify-content-between device-header"
        { style: H.css { minWidth: "11.5em" } } $
        [ titleElement ] <> headerComponents
    , Bootstrap.accordionHeader {} "detailed settings"
    , Bootstrap.accordionBody {} $
      [ summaryElement
      --  , H.fragment $ NonEmpty.toArray $ (\c -> H.div "" (show c)) <$> deviceDetails.capabilities
      ] <> featureComponents
    ]


-- Group stuff from here below

renderCapability
  :: forall r
   . Dispatch Message
  -> Group
  -> { state :: Maybe ValueOnOff | r }
  -> Capability
  -> ReactElement
renderCapability dispatch group groupState = case _ of
  OnOff cap ->
    Components.onOffSwitch
      dispatch
      -- deliberately using Device.setTopic here as it's the exact
      -- same format and I should remind myself to approach this
      -- differently
      (PublishGroupMsg <<< MQTT.mkPublishMsg (Device.setTopic group.name) $ MQTT.state "TOGGLE")
      (Just groupState)
      cap

  Brightness cap ->
    Components.numberSlider
      dispatch
      (PublishDeviceMsg <<< MQTT.mkGenericPublishMsg (Device.setTopic group.name) "brightness") -- TODO
      "brightness"
      Nothing -- TODO
      cap

  ColorTemperature cap ->
    Components.numberSlider
      dispatch
      (PublishDeviceMsg <<< MQTT.mkGenericPublishMsg (Device.setTopic group.name) "color_temp") -- TODO
      "color_temp"
      Nothing -- TODO
      cap

  ColorTempStartup cap ->
    Components.numberSlider
      dispatch
      (PublishDeviceMsg <<< MQTT.mkGenericPublishMsg (Device.setTopic group.name) "color_temp_startup") -- TODO
      "color_temp_startup"
      Nothing -- TODO
      cap

-- ColorXY CapabilityDetails ->
-- ColorHue CapabilityDetails
-- ColorHex CapabilityDetails


---   Covering CapabilityDetails
---   ColorGradient CapabilityDetails
  -- GradientScene cap ->
  --   Components.enumSelector
  --     dispatch
  --     (PublishDeviceMsg
  --      <<< MQTT.mkGenericPublishMsg
  --      (setTopic device.name)
  --      (fromMaybe "CHECK_YOUR_PROPERTY" preset.property)
  --     )
  --     Nothing -- TODO
  --     preset

--
-- TODO (don't necessarily have components to represent these yet anyways)
--
---   Hue CapabilityDetails
---   Saturation CapabilityDetails
---   IlluminanceLux CapabilityDetails
---   X CapabilityDetails
---   Y CapabilityDetails
---   Switch CapabilityDetails
---   Contact CapabilityDetails
---   Occupancy CapabilityDetails
---   OccupancyTimeout CapabilityDetails
---   Temperature CapabilityDetails
---   Humidity CapabilityDetails
---   AirQuality CapabilityDetails
---   VOC CapabilityDetails
---   Position CapabilityDetails
---   -- not specified (i.e. manufacturer can do whatever)
---   -- as of Matter 1.4
---   PowerOnBehavior CapabilityDetails
---   Effect CapabilityDetails
---   LEDControl CapabilityDetails
---   LinkQuality CapabilityDetails
---   Battery CapabilityDetails
---   BatteryLow CapabilityDetails
---   Tamper CapabilityDetails
--

  c ->
    H.div "" $ H.text ("Hey got: " <> show c)

findGroupDeviceStates :: Group -> DeviceStates -> Array DeviceState
findGroupDeviceStates group deviceStates =
  let
    groupMemberIDs = Set.fromFoldable $ group.members <#> _.id
  in
   deviceStates
   # M.filterKeys (flip Set.member groupMemberIDs)
   >>> M.values
   >>> Array.fromFoldable

--
-- O((n*m)*o)
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
           case groupState, Array.null onValues, all identity onValues of
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

groupSummary :: Group -> State -> Dispatch Message -> ReactElement
groupSummary group _state@{ deviceStates } dispatch =
  H.div "group card mt-2 mx-2 resource-card"  $
    H.div "card-body p-2" $
      genericWithDetails
        -- Maybe DeviceState, need to update for Group?
        Nothing

        -- title
        (H.div "card-title" group.name)

        -- summary
        (H.ul ""
          [
                H.li "" $ H.text $ "Group id: " <> (show group.id)
              -- , H.li "" $ H.text $ "Group members: " <> (show group.members)
              -- , H.li "" $ H.text $ "Group on/off state: " <> (show $ groupState g)
              -- , H.li "" $ H.text $ "Member capabilities: " <> (show $ memberCapabilities group.members)
              -- , H.li "" $ H.text $ "Group scenes: " <> (show group.members)
          ]
        )

        -- header components
        ( group.members
          # Set.toUnfoldable <<< memberCapabilities
          # Array.filter
          (case _ of
              OnOff _cap -> true
              _ -> false
          )
          <#> renderCapability dispatch group (groupState group)
        )

          -- feature components
        [ H.div ""
          [ H.h4 "" $ H.text (show $ Set.size $ memberCapabilities $ group.members)
          , H.div "" renderedCapabilities
          ]
        ]

  where
    memberCapabilities :: Array GroupDevice -> Set Capability
    memberCapabilities =
      foldMap (Set.fromFoldable <<< _.capabilities <<< Device.details <<< _.device)

    groupState g = groupDevicesOnOffState g <<< findGroupDeviceStates g $ deviceStates

    renderedCapabilities :: Array ReactElement
    renderedCapabilities =
      (Set.toUnfoldable <<< memberCapabilities $ group.members) <#>
        renderCapability dispatch group (groupState group)
