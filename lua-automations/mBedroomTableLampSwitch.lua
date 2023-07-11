
mBedroomTableLamp = nil
mBedroomTableLampSwitch = nil

function isempty(s)
  return s == nil or s == ''
end

function setup()
   mBedroomTableLamp = register("0x001788010b981ba0")
   mBedroomTableLampSwitch = register("0x04cd15fffe6d3826")

   switchChan = subscribe(mBedroomTableLampSwitch.topic)
end

function loop()
   resp = switchChan()

   -- apparently this can be null sometimes
   if not isempty(resp.action)  then
      -- logDebugMsg("Action: " .. resp.action)

      if resp.action == "on" then
         publish(mBedroomTableLamp.topicSet, { state = "ON" })
      elseif resp.action == "off" then
         publish(mBedroomTableLamp.topicSet, { state = "OFF" })
      end
   end
end

