
local frontDoorSensor = nil
local frontDoorSensorChan = nil
local basementMirrorLight = nil
local livingRoomWindowLight = nil

function setup ()
   frontDoorSensor = register("0x0015bc001e00f658")
   basementMirrorLight = register("0xb4e3f9fffe14c707")
   livingRoomWindowLight = register("0x680ae2fffe4577ac")
   frontDoorSensorChan = subscribe(frontDoorSensor.topic)
end

function loop ()
   resp = frontDoorSensorChan()

   if resp.contact == false then
      logDebugMsg("hey someone opened the door")
      -- would like to have some helper that lets me run this and then
      -- reset prior state...maybe something like `publishAndRestore`?
      publish(basementMirrorLight.topicSet, { effect = "blink" })
      publish(livingRoomWindowLight.topicSet, { effect = "blink" })
   end

   sleep(1)
end
