-- Used by the "closes Lua interpreter states when a LuaScript
-- automation stops" integration test. Registers a __gc finalizer that
-- appends to a sentinel file (whose path is passed via the
-- LUA_CLOSE_SENTINEL_PATH environment variable): Lua runs finalizers
-- when objects are collected or when the interpreter state is closed
-- (lua_close runs all pending finalizers), so the sentinel file only
-- gains entries if the state that ran this script was actually
-- closed. Leaked (never-closed) states never run their finalizers.
--
-- Uses only Lua's standard C library functions (io.open, os.getenv) —
-- not hslua-registered Haskell functions — because during lua_close,
-- registered function userdata (and their StablePtrs) are finalized
-- before user objects, making registered functions unreliable to call
-- from __gc.

local sentinel = setmetatable({}, { __gc = function ()
  local path = os.getenv("LUA_CLOSE_SENTINEL_PATH")
  if path then
    local f = io.open(path, "a")
    if f then f:write("closed\n"); f:close() end
  end
end })

function setup ()
   chan = subscribe("testCloseTopic")
end

function loop ()
   local msg = chan()
end