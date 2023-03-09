
local mirrorLight = nil

function setup ()
   mirrorLight = register("0xb4e3f9fffe14c707")
end

function loop ()
   logDebugMsg("mirrorLight friendly name: " .. mirrorLight.name)
end
