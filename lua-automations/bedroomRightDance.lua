
local topic = "zigbee2mqtt/Bedroom Right Bedside Lamp/set"

publish(topic, "{\"state\": \"ON\"}")

-- publish(topic, "{\"hue_move\": 40}")

publishJSON(topic, { hue_move = 40 })

-- local cjson = require "cjson"
-- require "cjson"
-- local json = cjson.encode({
--     foo = "bar",
--     some_object = {},
--     some_array = cjson.empty_array
-- })
-- print(json)
