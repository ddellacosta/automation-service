

local baseLightId = "0x680ae2fffe4577ac"
local device = nil
local setTopic = nil

function setup ()
   logDebugMsg("setup")
   device = register(baseLightId)
   setTopic = "zigbee2mqtt/" .. device._name .. "/set"
   publish(setTopic, { state = "ON" })
end

function loop ()
   local r = math.random(1,255)
   local g = math.random(1,255)
   local b = math.random(1,255)

   logDebugMsg(
      "Sending msg w/color (" .. r .. "/" .. g .. "/" .. b .. ") to topic "
      .. setTopic
   )

   colorMsg = {
      transition = 1,
      color = {
         rgb = r .. "," .. g .. "," .. b
      }
   }

   publish(setTopic, colorMsg)

   sleep(2)
end

function cleanup ()
   logDebugMsg("cleanup")
end
