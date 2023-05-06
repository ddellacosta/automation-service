
local standingLamp = nil

function setup()
   standingLamp = registerGroup(5)
   publish(standingLamp.topicSet, { state = "ON" })
end
