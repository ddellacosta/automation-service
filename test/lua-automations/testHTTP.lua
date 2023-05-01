function setup()
   logDebugMsg("hey")

   logDebugMsg(os.date("%y-%m-%d %T"))

   dateResponse = httpGet("https://aa.usno.navy.mil/api/rstt/oneday?date=2023-5-01&coords=41.5020948,-73.982543")

   local sunset
   local sunrise

   for idx, val in ipairs(dateResponse.properties.data.sundata) do
      if val.phen == "Rise" then
         sunrise = val.time
      elseif val.phen == "Set" then
         sunset = val.time
      end
   end

   logDebugMsg("sunrise: " .. sunrise)
   logDebugMsg("sunset: " .. sunset)
end
