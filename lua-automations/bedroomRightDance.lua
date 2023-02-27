
local topic = "zigbee2mqtt/Bedroom Right Bedside Lamp/set"

publishJSON(topic, { state = "ON" })
publishJSON(topic, { hue_move = 40 })
publishJSON(topic, { hue_move = "stop" })

publishJSON(topic, { state = "OFF" })
