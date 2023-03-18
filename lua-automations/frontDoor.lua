
local frontDoorSensor = nil
local frontDoorSensorChan = nil
local basementMirrorLight = nil

function setup ()
   frontDoorSensor = register("0x0015bc001e00f658")
   basementMirrorLight = register("0xb4e3f9fffe14c707")
   frontDoorSensorChan = subscribe(frontDoorSensor.topic)
end

function loop ()
   resp = frontDoorSensorChan()

   if resp.contact then
      logDebugMsg("hey someone closed the door")
      -- would like to have some helper that lets me run this and then
      -- reset prior state...maybe something like `publishAndRestore`?
      publish(basementMirrorLight.setTopic, { effect = "blink" })
   end

   sleep(1)
end
