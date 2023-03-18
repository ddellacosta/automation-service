
local diningRoomSwitch = nil
local diningRoomTableLight = nil

-- for getting messages from the light switch
local switchChan = nil

function setup ()
   diningRoomSwitch = register("0x001788010c29f4db")
   diningRoomTableLight = register("0x001788010bfadf37")

   switchChan = subscribe(diningRoomSwitch.topic) 
end

function loop ()
   resp = switchChan()

   if resp.action and type(resp.action) == "string" then
      logDebugMsg("resp.action " .. resp.action)
      if resp.action == "on_press_release" then
         publish(diningRoomTableLight.topicSet, { state = "TOGGLE" })
      -- this button doesn't do anything for now
      -- elseif resp.action == "off_press_release" then
      --    publish(diningRoomTableLight.topicSet, { state = "ON" })
      elseif resp.action == "down_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = -10 })
      elseif resp.action == "up_press_release" then
         publish(diningRoomTableLight.topicSet, { brightness_step = 10 })
      end
   end

   microSleep(1000)
end

function cleanup ()
end
