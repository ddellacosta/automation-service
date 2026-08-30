-- Used by the "closes Lua interpreter states when a LuaScript
-- automation stops" integration test. Registers a __gc finalizer that
-- sends a sentinel message via sendMessage: Lua runs finalizers when
-- objects are collected or when the interpreter state is closed
-- (lua_close runs all pending finalizers), so the sentinel only
-- appears if the state that ran this script was actually closed.
-- Leaked (never-closed) states never run their finalizers.

local sentinel = setmetatable({}, { __gc = function () sendMessage({send = "Null", msg = {state = "closed"}}) end })

function setup ()
   chan = subscribe("testCloseTopic")
end

function loop ()
   local msg = chan()
end