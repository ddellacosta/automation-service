
# Design

automation-service is inspired by/steals a bit from both [Erlang/OTP](https://www.erlang.org/doc/design_principles/des_princ.html) as well as Arduino--among other things, I hoped referencing Arduino development could be inviting and help provide a bit of an anchor for makers as well as new programmers. I also want the system to feel like a playground, with the ability to rapidly iterate and try new ideas out without a lot of overhead.


## Architecture

automation-service consists of a central daemon that listens for messages to come in, and then performs actions based on those messages. These are defined in [`Service.Daemon.Messages`](https://github.com/ddellacosta/automation-service/blob/5449d50b6b34d31b537f2fdae01908036ba2fe42/src/Service/Messages/Daemon.hs):

* `Start AutomationName`
* `Stop AutomationName`
* `SendTo AutomationName AutomationMessage`
* `Schedule Message AutomationSchedule`

`AutomationMessage` is a JSON value.

`AutomationSchedule` is a cron-formatted string, like "* * * * *". [This](https://crontab.guru/) is handy.

`Message` in `Schedule` is itself a server message, e.g. `Schedule (Start Gold) "* * * * *"`.

The following `Message` types are internal, used either by the Lua API or device auto-registration system. `Null` is simply a no-op message that failure cases or tests can use to exercise the infrastructure without triggering any behavior from the `Daemon`.

```haskell
DeviceUpdate :: [Device] -> Message
Register :: DeviceId -> AutomationName -> Message
Subscribe :: Maybe Topic -> TChan Value -> Message
Null :: Message
```


## LuaScript

The `LuaScript` automation is actually "just" a Haskell automation. The entire Lua API is created and loaded in the `LuaScript` automation, and the term `LuaScript "luaScriptName"` is an `AutomationName` as described by the `Message` type discussed above. Managing automations in decoupled from any Lua-specific logic.


## Devices, Groups, Scenes

Write now automation-service is heavily coupled to Zigbee2MQTT at this point, but I hope to integrate ESPHome soon as well. TODO: fill this section out more 


## Why not use HASS YAML/Python scripts/AppDaemon/NodeRed?

I think Python is probably a great choice for writing automations given the amount of functionality available for it in the form of libraries and whatnot, I just personally hate it. I also think YAML is generally terrible. I'm not much interested in visual programming paradigms like NodeRed either.

None of this is meant as criticisms of any of these projects (well, leaving aside YAML lol). I think they are great projects and I've gotten a lot of use out of HA in particular. I continue to use it for dashboard building and easy integration with all kinds of services and protocols, and more. It also seems clear that a lot of people find a lot of utility with NodeRed and AppDaemon. On the contrary, I hope I can contribute something back to the open source Home Automation community in gratitude.

I also wanted to build something more substantial with Haskell, and this seemed like a good experiment. However at some point I realized that, while I love Haskell, having to recompile and rebuild my docker image every time I wanted to add automations was quite tiresome. Then I discovered the incredible [HsLua](https://hslua.org/) project and [Lua](http://www.lua.org/) itself, and at that point this thing kind of took on a life of its own.

