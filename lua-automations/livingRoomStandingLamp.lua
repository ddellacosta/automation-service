
function sleep(n)
  os.execute("sleep " .. tonumber(n))
end

local livingRoomStandingLamp1Topic = "zigbee2mqtt/Living Room Standing 40W 1/set"
local livingRoomStandingLamp2Topic = "zigbee2mqtt/Living Room Standing 40W 2/set"
local livingRoomStandingLamp3Topic = "zigbee2mqtt/Living Room Standing 40W 3/set"

publishJSON(livingRoomStandingLamp1Topic, { state = "ON" })
publishJSON(livingRoomStandingLamp2Topic, { state = "ON" })
publishJSON(livingRoomStandingLamp3Topic, { state = "ON" })

math.randomseed(os.time())

function changeBulbColor (bulbTopic)
   local red = math.random(1,255)
   local blue = math.random(1,255)
   local green = math.random(1,255)

   publishJSON(
      bulbTopic, {
         transition = 2,
         color = {
            rgb = red .. "," .. blue .. "," .. green
         }
      }
   )
end

while (true) do
   changeBulbColor(livingRoomStandingLamp1Topic)
   changeBulbColor(livingRoomStandingLamp2Topic)
   changeBulbColor(livingRoomStandingLamp3Topic)
   sleep(6)
end
