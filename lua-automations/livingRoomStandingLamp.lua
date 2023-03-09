
local standingLampBulbTopId = "0x001788010c9941c5"
local standingLampBulbMidId = "0x001788010c3b2e53"
local standingLampBulbLowId = "0x001788010c3b38af"

local standingLampBulbTop = nil
local standingLampBulbMid = nil
local standingLampBulbLow = nil

-- does it make sense to have this here?
math.randomseed(os.time())

function setup ()
   standingLampBulbTop = register(standingLampBulbTopId)
   standingLampBulbMid = register(standingLampBulbMidId)
   standingLampBulbLow = register(standingLampBulbLowId)

   publish(standingLampBulbTop, { state = "ON" })
   publish(standingLampBulbMid, { state = "ON" })
   publish(standingLampBulbLow, { state = "ON" })
end

function changeBulbColor (bulbTopic)
   local red = math.random(1,255)
   local blue = math.random(1,255)
   local green = math.random(1,255)

   publish(
      bulbTopic, {
         transition = 2,
         color = {
            rgb = red .. "," .. blue .. "," .. green
         }
      }
   )
end

function loop ()
   changeBulbColor(standingLampBulbTop._setTopic)
   changeBulbColor(standingLampBulbMid._setTopic)
   changeBulbColor(standingLampBulbLow._setTopic)
   sleep(6)
end
