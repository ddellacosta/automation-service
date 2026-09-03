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