function loopAutomation ()
  topicChannel = subscribe("a/b/c")
  sleep(1)
  response = topicChannel()
  logDebugMsg("Msg: " .. response.msg)
end
