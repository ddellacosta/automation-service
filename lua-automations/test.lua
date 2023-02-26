
-- this breaks Lua in Haskell with ErrRun
-- local cjson = require "cjson"
-- require "cjson"
-- local json = cjson.encode({
--     foo = "bar",
--     some_object = {},
--     some_array = cjson.empty_array
-- })
-- print(json)

-- this works
-- local file = io.open( "testout.txt", "w" )
-- file:write( "here's a line\n" )
-- file:write( "and another\n" )
-- file:write( "and yet another\n" )
-- file:close()


logDebugMsg("hell yeah I'm logging from Lua")

publish()
