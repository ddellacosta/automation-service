
local livingRoomMultiSwitch = nil
local diningRoomTableLight = nil
local livingRoomLightStrip = nil
-- TODO add the group for these here too somehow
local standingLampBulbTop = nil
local standingLampBulbMid = nil
local standingLampBulbLow = nil

-- for getting messages from the multi-light switch
local switchChan = nil

-- for determining which device(s) to apply dial actions to
local lastAction = nil

function setup ()
   livingRoomMultiSwitch = register("0x001788010d35f25f")
   diningRoomTableLight = register("0x001788010bfadf37")
   livingRoomLightStrip = register("0x680ae2fffe4577ac")
   standingLampBulbTop = register("0x001788010c9941c5")
   standingLampBulbMid = register("0x001788010c3b2e53")
   standingLampBulbLow = register("0x001788010c3b38af")

   switchChan = subscribe(livingRoomMultiSwitch.topic) 
end

function loop ()
   resp = switchChan()

   if resp.action == "button_1_press_release" then
      publish(livingRoomLightStrip.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_2_press_release" then
      publish(diningRoomTableLight.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_3_press_release" then
      publish(standingLampBulbTop.topicSet, { state = "TOGGLE" })
      publish(standingLampBulbMid.topicSet, { state = "TOGGLE" })
      publish(standingLampBulbLow.topicSet, { state = "TOGGLE" })
   elseif resp.action == "button_4_press_release" then
      publish(livingRoomLightStrip.topicSet, { state = "TOGGLE" })
      publish(diningRoomTableLight.topicSet, { state = "TOGGLE" })
      publish(standingLampBulbTop.topicSet, { state = "TOGGLE" })
      publish(standingLampBulbMid.topicSet, { state = "TOGGLE" })
      publish(standingLampBulbLow.topicSet, { state = "TOGGLE" })
   elseif resp.action == "dial_rotate_left_step" or resp.action == "dial_rotate_left_slow" then
      if lastAction == "button_1_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_2_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_3_press_release" then
         publish(standingLampBulbTop.topicSet, { brightness_step = -10 })
         publish(standingLampBulbMid.topicSet, { brightness_step = -10 })
         publish(standingLampBulbLow.topicSet, { brightness_step = -10 })
      elseif lastAction == "button_4_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = -10 })
         publish(diningRoomTableLight.topicSet, { brightness_step = -10 })
         publish(standingLampBulbTop.topicSet, { brightness_step = -10 })
         publish(standingLampBulbMid.topicSet, { brightness_step = -10 })
         publish(standingLampBulbLow.topicSet, { brightness_step = -10 })
      end
   elseif resp.action == "dial_rotate_right_step" or resp.action == "dial_rotate_right_slow" then
      if lastAction == "button_1_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_2_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_3_press_release" then
         publish(standingLampBulbTop.topicSet, { brightness_step = 10 })
         publish(standingLampBulbMid.topicSet, { brightness_step = 10 })
         publish(standingLampBulbLow.topicSet, { brightness_step = 10 })
      elseif lastAction == "button_4_press_release" then
         publish(livingRoomLightStrip.topicSet, { brightness_step = 10 })
         publish(diningRoomTableLight.topicSet, { brightness_step = 10 })
         publish(standingLampBulbTop.topicSet, { brightness_step = 10 })
         publish(standingLampBulbMid.topicSet, { brightness_step = 10 })
         publish(standingLampBulbLow.topicSet, { brightness_step = 10 })
      end
   end

   if (resp.action == "button_1_press_release" or
       resp.action == "button_2_press_release" or
       resp.action == "button_3_press_release" or
       resp.action == "button_4_press_release"
   )
   then
      lastAction = resp.action
   end

--    if lastAction then
--       logDebugMsg("lastAction: " .. lastAction)
--    end

   if resp.action then
      logDebugMsg("this action: " .. resp.action)
      if lastAction then
         logDebugMsg("lastAction: " .. lastAction)
      end
   end

--    logDebugMsg("is it just the thread (+ getting subscription msg) that dies?")

   microSleep(10000)

   -- sleep(1)
end
