function setup()
   local latitude = 41.5020948
   local longitude = -73.982543
   local sunEvents = getSunEvents(latitude, longitude)

   logDebugMsg("sunrise: " .. sunEvents.sunrise)
   logDebugMsg("sunset: " .. sunEvents.sunset)

   local updatedSunrise = addMinutes(30, sunEvents.sunrise)
   local updatedSunset = addMinutes(-30, sunEvents.sunset)

   logDebugMsg("updatedSunrise: " .. updatedSunrise)
   logDebugMsg("updatedSunset: " .. updatedSunset)

   local cronifiedNewRise = timestampToCron(updatedSunrise)
   local cronifiedNewSet = timestampToCron(updatedSunset)

   logDebugMsg("cronifiedNewRise: " .. cronifiedNewRise)
   logDebugMsg("cronifiedNewSet: " .. cronifiedNewSet)
end
