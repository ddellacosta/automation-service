
local motionSensor = nil
local gymCeilingLights = nil

-- for getting messages from the light switch
local switchChan = nil

function setup ()
   motionSensor = register("0x0015bc001a02709e")
   -- gymCeilingLights = register("0x001788010bfadf37")

   switchChan = subscribe(motionSensor.topic) 
end

function loop ()
   resp = switchChan()

   if resp.occupancy and type(resp.occupancy) == "boolean" then
      if resp.occupancy then
        logDebugMsg("resp.occupancy is true, turning on gym lights.")
        publish("zigbee2mqtt/Gym Ceiling - All/set", { state = "ON" })
      end
   end

   microSleep(1000)
end

function cleanup ()
end
