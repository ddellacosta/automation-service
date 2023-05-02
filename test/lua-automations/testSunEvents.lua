function setup()
   local coords = "41.5020948,-73.982543"
   local sunEvents = getSunEvents(coords)
   logDebugMsg("sunrise: " .. sunEvents.sunrise)
   logDebugMsg("sunset: " .. sunEvents.sunset)

   local updatedSunrise = addMinutes(30, sunEvents.sunrise)
   local updatedSunset = addMinutes(-30, sunEvents.sunset)

   logDebugMsg("updatedSunrise: " .. updatedSunrise)
   logDebugMsg("updatedSunset: " .. updatedSunset)
end
