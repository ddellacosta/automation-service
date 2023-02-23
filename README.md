# automation-service

automation-service is a tool for setting up schedulable automations written in Lua or Haskell, which runs as a networked service, in the spirit of Home Assistant and AppDaemon, filling much the same niche as the latter. Its (so-far) primary use-case is as a `docker-compose`-embedded network service set up with [Home-Assistant](https://www.home-assistant.io/installation/linux#install-home-assistant-container) and tools like zigbee2mqtt/zwave2mqtt as an alternative way for buildng complex automations for Home Assistant, triggerable and schedulable via MQTT. 

Right now it's in super pre-alpha state, but you can already start, stop, and schedule complex automations written in Lua or Haskell.


## TODOs

## Why not use HASS YAML/Python scripts/AppDaemon?

Simply put, I don't like the ergonomics. I find a lot of YAML I see in HASS gets hairy fast with confusing semantics when it is doing anything complex, and as far as Python, being a fan of FP, I think a lot of it can be explained by the great answers in this SO post: https://stackoverflow.com/questions/1017621/why-isnt-python-very-good-for-functional-programming.

None of this is a knock on Home Assistant or AppDaemon--in fact, I have a tremendous amount of respect for these projects. It's important to understand that all of these choices have different tradeoffs. For example, Python is an excellent choice for home automation because of the amount of freely accessible code and libraries available in that ecosystem--encompassing everything from AI to low-level interfacing with electronics. A lot of people also feel like it is comparatively easy to pick up (similarly to Lua). The point is, there are always tradeoffs, and I think there is room in the open home automation landscape for more tools with different tradeoffs in this niche.

Regarding AppDaemon in particular, I found [this](https://appdaemon.readthedocs.io/en/latest/HASS_TUTORIAL.html#another-take-on-automation) to be such an excellent statement of effective utility for my project that I resolve to work toward these in automation-service as well:

    So given the importance of Automation, what should Automation allow us to do? I am a pragmatist at heart, so I judge individual systems by the ease of accomplishing a few basic but representative tasks:
    
        Can the system respond to the presence or absence of people?
        Can I turn a light on at Sunset +/- a certain amount of time?
        Can I arrive home in light or dark and have the lights figure out if they should be on or off?
        As I build my system out, can I get the individual pieces to co-operate and use and reuse (potentially complex) logic to make sure everything works smoothly?
        Is it open and expandable?
        Does it run locally without any reliance on the cloud?
    
    In my opinion, Home Assistant accomplishes the majority of these very well with a combination of Automations, Scripts, and Templates, and its Restful API.

I think I've established a solid foundation already in this project given the bottom three considerations, and I aim to provide the kind of tool that lets folks easily set up, configure, and extend the functionality described by the first three points. I think the main point of distinction between this project and AppDaemon is described in the section "So why AppDaemon?" wrt to the "All the power of Python" part and that alone--I'm trying something different using Haskell and Lua, so this results in different tradeoffs--maybe I'll be able to provide better safety and refactoring power, speed, and whatever subjective benefits using the tools I'm using provides--but these differences are the least important in all of these lists. The goal is "Open source home automation that puts local control and privacy first." ((Where have I heard that before?)[https://www.home-assistant.io/blog/2016/01/19/perfect-home-automation/])


## Testing

Running tests:

```bash
$ watchexec -w test -w src -w app 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```

## License/Copyright

GNU AGPLv3 License. Copyright © 2022-2023 Dave Della Costa
