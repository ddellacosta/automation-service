# Lua API

* [Core structure](#core-structure)
  * [setup](#setup)
  * [loop](#loop)
  * [cleanup](#cleanup)

* [Scripting helpers](#scripting-helpers)
  * [addMinutes](#addMinutes)
  * [getSunEvents](#getSunEvents)
  * [httpGet](#httpGet)
  * [logDebugMsg](#logDebugMsg)
  * [microSleep](#microSleep)
  * [publish](#publish)
  * [register](#register)
  * [registerGroup](#registerGroup)
  * [sendMessage](#sendMessage)
  * [sleep](#sleep)
  * [subscribe](#subscribe)
  * [timestampToCron](#timestampToCron)

## Core structure

None of the core functions are technically required, but a script without any of them will just end (unless you want to draw outside the lines with Lua...do it, and let me know how it turns out (seriously)).

### setup

When you implement the setup function, anything included in this function will be run once, when the automation script starts.

```lua
function setup ()
   logDebugMsg("This is run once.")
end
```

### loop

A loop implementation will run after `setup()`, and repeat continuously until the automation script is explicitly stopped.

```lua
function setup ()
   logDebugMsg("This is run once.")
end

function loop ()
   logDebugMsg("This is run over and over.")
end
```

### cleanup

The cleanup function is run once when the script either ends naturally (due to not having a `loop()` implementation) or when it is explicitly stopped.

```lua
function setup ()
   logDebugMsg("This is run once.")
end

function loop ()
   logDebugMsg("This is run over and over.")
end

function cleanup ()
   logDebugMsg("This runs when the automation stops, or is stopped.")
end
```


## Scripting helpers

### addMinutes

Takes a timestamp like those returned by getSunEvents, and adds minutes.

```lua
   logDebugMsg("sunset: " .. sunEvents.sunset)
   -- log output:
   -- Debug - 10/May/2023:20:03:45 +0000 - sunsetLighting: sunset: 2023-05-11T00:03:09.731673896312Z

   -- local thirtyBeforeSunset = addMinutes(-30, sunEvents.sunset)
   -- Debug - 10/May/2023:20:03:45 +0000 - sunsetLighting: thirtyBeforeSunset: 2023-05-10T23:33:09.731673896312Z
```

### getSunEvents

Given a latitude and longitude pair, calculates the sunrise and sunset for the current location.

```lua
   local latitude = 41.5020948
   local longitude = -73.982543
   local sunEvents = getSunEvents(latitude, longitude)

   -- returns a data structure with values at sunEvents.sunrise and sunEvents.sunset
```


### httpGet (experimental)

Takes a url string and returns whatever JSON is returned. Currently very alpha quality in terms of error-handling and capabilities.


### logDebugMsg

Takes a string and logs a message to the automation-service log, at Debug log level.

```lua
   -- myScript.lua
   function setup()
     logDebugMsg("hey, look at me, I'm a log message")
   end

   -- Log output:
   -- Debug - 10/May/2023:20:01:45 +0000 - myScript: hey, look at me, I'm a log message 
```


### microSleep

Sleeps for a given number of microseconds. Corresponds directly to Haskell's [threadDelay](https://hackage.haskell.org/package/unliftio-0.2.24.0/docs/UnliftIO-Concurrent.html#v:threadDelay).


### publish

Publish will send a JSON message to a given topic.

```lua
function loop ()
   resp = switchChan()

   if resp.action and type(resp.action) == "string" then
      if resp.action == "on_press_release" then
         publish(diningRoomTableLight.topicSet, { state = "TOGGLE" })
-- ...
end

-- or an automation-service internal message:
```


### register

Register a device, returning a data structure including topics for publishing and subscribing. Expects a Zigbee2MQTT IEEE Address as an argument, and returns a data structure including information about the device:

```lua
function setup ()
   frontDoorSensor = register("0x0015bc001e00f658")
   frontDoorSensorChan = subscribe(frontDoorSensor.topic)
end
```

This includes the string-valued data attributes `id`, `name`, `category`, `manufacturer` (possibly `nil`), `model` (possibly `nil`), `topic`, `topicGet`, `topicSet`.


### registerGroup

Same as `register` for devices, but expects a Zigbee2MQTT group id, and the attributes returned include: `id`, `name`, `members` (an array of data including `memberId` and `endPoint`), `scenes` (an array of data including `sceneId` and `sceneName`), `topic`, `topicGet`, `topicSet`


### sendMessage

Sends a message to the automation-service daemon, per the messaging protocol (this is not sent via the MQTT broker, but internally):

```lua
   sendMessage({
      jobId = "sunsetLightsOn",
      schedule = "33 23 10 05 3"
      job = { start = "sunsetLightsOn" }
   })
```


### sleep

Sleeps for a given number of seconds. Corresponds to Haskell's [threadDelay](https://hackage.haskell.org/package/unliftio-0.2.24.0/docs/UnliftIO-Concurrent.html#v:threadDelay), preceded by a conversion to microseconds.


### subscribe

Given a topic, subscribes to that topic, returning a blocking channel for listening for topic messages coming in.

```lua
local frontDoorSensor
local frontDoorSensorChan

function setup ()
   frontDoorSensor = register("0x0015bc001e00f658")
   frontDoorSensorChan = subscribe(frontDoorSensor.topic)
end

function loop ()
   resp = frontDoorSensorChan()

   if resp.contact == false then
      logDebugMsg("hey someone opened the door")
      -- ...
   end
end
```


### timestampToCron

Given a timestamp, returns a "cron instant" corresponding to that timestamp--for setting a job at a specific moment in time.

```lua
   logDebugMsg("thirtyBeforeSunset: " .. thirtyBeforeSunset)
   -- Debug - 10/May/2023:20:03:45 +0000 - sunsetLighting: thirtyBeforeSunset: 2023-05-10T23:33:09.731673896312Z

   local thirtyBeforeSunsetCron = timestampToCron(thirtyBeforeSunset)
   -- Debug - 10/May/2023:20:03:45 +0000 - sunsetLighting: thirtyBeforeSunsetCron: 33 23 10 05 3
```
