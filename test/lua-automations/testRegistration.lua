
local mirrorLight = nil
local basementStandingLamp = nil

function setup ()
   mirrorLight = register("0xb4e3f9fffe14c707")
   basementStandingLamp = registerGroup(1)
end

function loop ()
   logDebugMsg("mirrorLight friendly name: " .. mirrorLight.name)
   logDebugMsg("basementStandingLamp friendly name: " .. basementStandingLamp.name)
   -- Pace the loop. Without a sleep this is a tight grind: two
   -- Lua->Haskell calls per iteration with nothing to slow it down,
   -- burning CPU and growing the log unboundedly for as long as the
   -- automation lives. It also amplifies delayed cancellation -- a Lua
   -- automation blocked inside a foreign call cannot receive
   -- AsyncCancelled until the call returns, so a leftover instance of
   -- this automation can keep grinding long after shutdown was
   -- requested (see the note on cleanupAutomations in
   -- src/Service/Daemon.hs). The other fixtures sleep in their loops
   -- as well.
   sleep(1)
end
