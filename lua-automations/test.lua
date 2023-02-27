
function sleep(n)
  os.execute("sleep " .. tonumber(n))
end

logDebugMsg("hell yeah I'm logging from Lua")

local livingRoomLightStripTopic = "zigbee2mqtt/Living Room Window Base Light Strip/set"

publishJSON(livingRoomLightStripTopic, { state = "ON" })

math.randomseed(os.time())


while (true) do
   local red = math.random(1,255)
   local blue = math.random(1,255)
   local green = math.random(1,255)

   publishJSON(
      livingRoomLightStripTopic, {
         color = {
            rgb = red .. "," .. blue .. "," .. green
         }
      }
   )
   sleep(5)
end
