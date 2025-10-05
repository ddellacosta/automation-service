
local topicChannel = nil

function setup ()
  topicChannel = subscribe("testTopic")
end

function loop ()
   response = topicChannel()
   logDebugMsg("Msg: " .. response.msg)

   sleep(1)
end

function cleanup()
end
