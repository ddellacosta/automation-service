function setup()
   local latitude = 41.5020948
   local longitude = -73.982543
   local sunEvents = getSunEvents(latitude, longitude)

   -- logDebugMsg("sunrise: " .. sunEvents.sunrise)
   logDebugMsg("sunset: " .. sunEvents.sunset)

   local thirtyBeforeSunset = addMinutes(-30, sunEvents.sunset)

   logDebugMsg("thirtyBeforeSunset: " .. thirtyBeforeSunset)

   local thirtyBeforeSunsetCron = timestampToCron(thirtyBeforeSunset)

   logDebugMsg("thirtyBeforeSunsetCron: " .. thirtyBeforeSunsetCron)

   sendMessage({
      jobId = "sunsetLightsOn",
      schedule = thirtyBeforeSunsetCron,
      job = { start = "sunsetLightsOn" }
   })
end
