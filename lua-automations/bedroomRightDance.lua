
local topic = "zigbee2mqtt/Bedroom Right Bedside Lamp/set"

publishJSON(topic, { state = "ON" })

-- publish(topic, "{\"hue_move\": 40}")

publishJSON(topic, { hue_move = 40 })
publishJSON(topic, { hue_move = "stop" })
