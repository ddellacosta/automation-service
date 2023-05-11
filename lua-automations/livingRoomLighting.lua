
-- devices
local livingRoomMultiSwitch = nil
local diningRoomTableLight = nil
local livingRoomLightStrip = nil

-- groups
local standingLamp = nil
local livingRoomLighting = nil

-- for getting messages from the multi-light switch
local switchChan = nil

-- for determining which device(s) to apply dial actions to
local lastAction = nil

function setup ()
   livingRoomMultiSwitch = register("0x001788010d35f25f")
   diningRoomTableLight = register("0x001788010bfadf37")
   livingRoomLightStrip = register("0x680ae2fffe4577ac")
   standingLamp = registerGroup(5)
   livingRoomLighting = registerGroup(2)

   switchChan = subscribe(livingRoomMultiSwitch.topic) 
end

function loop ()
   resp = switchChan()

   -- I am not a very good Lua programmer, yet
   if resp.action == "button_1_press_release" then
      publish(livingRoomLightStrip.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_2_press_release" then
      publish(diningRoomTableLight.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_3_press_release" then
      publish(standingLamp.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_4_press_release" then
      publish(livingRoomLighting.topicSet, { state = "TOGGLE" })
   elseif resp.action == "dial_rotate_left_step" or resp.action == "dial_rotate_left_slow" then
      if lastAction == "button_1_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_2_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_3_press_release" then
         publish(standingLamp.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_4_press_release" then
         publish(livingRoomLighting.topicSet, { brightness_step = -10 })
      end
   elseif resp.action == "dial_rotate_right_step" or resp.action == "dial_rotate_right_slow" then
      if lastAction == "button_1_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_2_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_3_press_release" then
         publish(standingLamp.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_4_press_release" then
         publish(livingRoomLighting.topicSet, { brightness_step = 10 })
      end
   end

   if (resp.action == "button_1_press_release" or
       resp.action == "button_2_press_release" or
       resp.action == "button_3_press_release" or
       resp.action == "button_4_press_release")
   then
      lastAction = resp.action
   end

   microSleep(1000)
end
