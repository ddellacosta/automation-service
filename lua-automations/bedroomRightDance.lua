
local bedsideRightLampId = "0x001788010cfb9ddb"
local bedsideRightLamp = nil

function setup ()
   bedsideRightLamp = register(bedsideRightLampId)
   publish(bedsideRightLamp.topicSet, { state = "ON" })
   publish(bedsideRightLamp.topicSet, { hue_move = 40 })
end

function cleanup ()
   publish(bedsideRightLamp.topicSet, { hue_move = "stop" })
end
