

function loopAutomation ()
  local mirrorLight = register("0xb4e3f9fffe14c707")
  
  logDebugMsg("mirrorLight friendly name: " .. mirrorLight._name)
  
  topicChannel = subscribe("a/b/c")

  logDebugMsg("okay can I log at all after calling subscribe")

  response = topicChannel()
  sleep(1)

  logDebugMsg("Msg: " .. response.msg)

  -- publish(topic, { test = "works" })

end
