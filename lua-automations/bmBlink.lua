
local mirrorLightId = "0xb4e3f9fffe14c707"
local mirrorLight = nil

function setup ()
   mirrorLight = register(mirrorLightId)
   publish(mirrorLight.topicSet, { state = "ON" })

   math.randomseed(os.time())

   local r = math.random(1,255)
   local g = math.random(1,255)
   local b = math.random(1,255)

   logDebugMsg(
      "blimk " .. mirrorLight.topicSet ..
      " guh-huh1! " .. r .. "," .. g .. "," .. b
   )

   blinkMsg = {
      effect = "blink"
   }

   publish(mirrorLight.topicSet, blinkMsg)

   sleep(1)

   colorMsg = {
      transition = 2,
      color = {
         rgb = r .. "," .. g .. "," .. b
      }
   }

   publish(mirrorLight.topicSet, colorMsg)
end
