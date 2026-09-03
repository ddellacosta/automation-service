local sentinel = setmetatable({}, { __gc = function ()
  local path = os.getenv("LUA_CLOSE_SENTINEL_PATH")
  if path then
    local f = io.open(path, "a")
    if f then f:write("closed\n"); f:close() end
  end
end })

function setup ()
   logDebugMsg("testCloseFinalizer setup")
end

-- intentionally no loop function: the automation completes on its own
-- (see the Haskell test's comment block for why that matters)