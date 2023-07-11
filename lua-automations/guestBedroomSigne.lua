
guestBedroomSigne = nil
guestBedroomSigneSwitch = nil

function setup()
   guestBedroomSigne = register("0x001788010c5276fb")
   guestBedroomSigneSwitch = register("0x040d84fffecbabb4")

   switchChan = subscribe(guestBedroomSigneSwitch.topic)
end

function loop()
   resp = switchChan()

   logDebugMsg("Action: " .. resp.action)

   if resp.action == "on" then
      publish(guestBedroomSigne.topicSet, { state = "ON" })
   elseif resp.action == "off" then
      publish(guestBedroomSigne.topicSet, { state = "OFF" })
   elseif resp.action == "arrow_right_click" then
      publish(guestBedroomSigne.topicSet, { brightness_step = 10 })
   elseif resp.action == "arrow_left_click" then
      publish(guestBedroomSigne.topicSet, { brightness_step = -10 })
   end
end
