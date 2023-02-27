
-- this breaks Lua in Haskell with ErrRun
-- local cjson = require "cjson"
-- require "cjson"
-- local json = cjson.encode({
--     foo = "bar",
--     some_object = {},
--     some_array = cjson.empty_array
-- })
-- print(json)

-- this works
-- local file = io.open( "testout.txt", "w" )
-- file:write( "here's a line\n" )
-- file:write( "and another\n" )
-- file:write( "and yet another\n" )
-- file:close()

--require "posix"

logDebugMsg("hell yeah I'm logging from Lua")

-- posix.sleep(3)

-- logDebugMsg("sleeped?")

function sleep(n)
  os.execute("sleep " .. tonumber(n))
end

local livingRoomLightStripTopic = "zigbee2mqtt/Living Room Window Base Light Strip/set"

publish(livingRoomLightStripTopic, "{\"state\": \"ON\"}")

math.randomseed(os.time())


while (true) do
   local red = math.random(1,255)
   local blue = math.random(1,255)
   local green = math.random(1,255)
   publish(livingRoomLightStripTopic, "{\"color\":{\"rgb\":\"" .. red .. "," .. blue .. "," .. green .."\"}}")
   sleep(5)
end
