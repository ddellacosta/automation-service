function setup()
   local latitude = 41.5020948
   local longitude = -73.982543
   local sunEvents = getSunEvents(latitude, longitude)

   -- logDebugMsg("sunrise: " .. sunEvents.sunrise)
   logDebugMsg("sunset: " .. sunEvents.sunset)

   local seventyFiveBeforeSunset = addMinutes(-75, sunEvents.sunset)

   logDebugMsg("seventyFiveBeforeSunset: " .. seventyFiveBeforeSunset)

   local seventyFiveBeforeSunsetCron = timestampToCron(seventyFiveBeforeSunset)

   logDebugMsg("seventyFiveBeforeSunsetCron: " .. seventyFiveBeforeSunsetCron)

   sendMessage({
      jobId = "sunsetLightsOn",
      schedule = seventyFiveBeforeSunsetCron,
      job = { start = "sunsetLightsOn" }
   })
end
