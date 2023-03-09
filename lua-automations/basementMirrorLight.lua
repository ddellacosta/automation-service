
local mirrorLightId = "0xb4e3f9fffe14c707"
local device = nil

function setup ()
   logDebugMsg("setup")
   device = register(mirrorLightId)
   publish(device.setTopic, { state = "ON" })
end

function loop ()
   local r = math.random(1,255)
   local g = math.random(1,255)
   local b = math.random(1,255)

   logDebugMsg(
      "Sending msg w/color (" .. r .. "/" .. g .. "/" .. b .. ") to topic "
      .. device.setTopic
   )

   colorMsg = {
      transition = 2,
      color = {
         rgb = r .. "," .. g .. "," .. b
      }
   }

   publish(device.setTopic, colorMsg)

   sleep(5)
end

function cleanup ()
   logDebugMsg("cleanup")
end
