
local mirrorLight = nil
local basementStandingLamp = nil

function setup ()
   mirrorLight = register("0xb4e3f9fffe14c707")
   basementStandingLamp = registerGroup(1)
end

function loop ()
   logDebugMsg("mirrorLight friendly name: " .. mirrorLight.name)
   logDebugMsg("basementStandingLamp friendly name: " .. basementStandingLamp.name)
end
