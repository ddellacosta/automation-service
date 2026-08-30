-- Used by the "closes Lua interpreter states when a LuaScript
-- automation stops" integration test. Registers a __gc finalizer that
-- logs when it runs: Lua runs finalizers when objects are collected
-- or when the interpreter state is closed (lua_close runs all pending
-- finalizers), so the log line only appears if the state that ran
-- this script was actually closed. Leaked (never-closed) states never
-- run their finalizers.

local sentinel = setmetatable({}, { __gc = function () logDebugMsg("lua state closed") end })

function setup ()
   chan = subscribe("testCloseTopic")
end

function loop ()
   local msg = chan()
end