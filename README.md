# automation-service

<img style="width: 45%" src="docs/ha-custom-card.png" alt="screenshot of HA custom card" />

automation-service is a tool for setting up simple-to-complicated automations. Its goals include:
  * allowing quick feedback loops for experimentation and testing
  * robustness, stability, efficiency
  * supporting open standards and interoperability
  * **being simple to understand and start using for home automation DIYers and makers**

It communicates via MQTT and can automatically pull and use device and group information from Zigbee2MQTT, but it can potentially be used with anything that can communicate over MQTT. It provides a simple message protocol for starting, stopping, and scheduling automations over MQTT, and lets you write automations in Lua with [batteries-included helpers](docs/lua_api.md) by dropping scripts in the `lua-automations` directory. There is a custom card available for showing status information (and more soon) in Home Assistant as well, seen in the above screenshot.

Right now this software is usable, but in a very alpha state. See [TODO](#todo) below.


## How It Works

Currently it is assumed you will be running automation-service in a container setup [as described below](#running). I'd like to figure out how to add it as an integration in Home Assistant soon as well.

In the root directory of your automation-service install (as configured in docker-compose), in the same location as the config.dhall file, place a directory called `lua-automations`--the easy way to set all of this up is to simply clone/copy the repo into place and tweak.

Any Lua scripts in the `lua-automations` directory will be available for running. Here is a basic example which flashes lights when a sensor is triggered and a specific condition is met:

```lua
local frontDoorSensor
local frontDoorSensorChan
local basementMirrorLight
local livingRoomWindowLight

function setup ()
   frontDoorSensor = register("0x0015bc001e00f658")
   basementMirrorLight = register("0xb4e3f9fffe14c707")
   livingRoomWindowLight = register("0x680ae2fffe4577ac")
   frontDoorSensorChan = subscribe(frontDoorSensor.topic)
end

function loop ()
   resp = frontDoorSensorChan()

   if resp.contact == false then
      logDebugMsg("hey someone opened the door")
      publish(basementMirrorLight.topicSet, { effect = "blink" })
      publish(livingRoomWindowLight.topicSet, { effect = "blink" })
   end
end
```

You can start this by sending a "start" message to the topic "automation-service/set", e.g.:

```bash
$ mosquitto_pub -u user -p password -t automation-service/set -m "{\"start\": \"frontDoor\"}"
```

A second "start" message will kill any running instances of the automation, and start it again (setup() will be run again). Similarly, you can stop a running automation using a "stop" message:


```bash
$ mosquitto_pub -u user -p password -t automation-service/set -m "{\"stop\": \"frontDoor\"}"
```

You can schedule an automation to run with a message as well, using [cron](https://en.wikipedia.org/wiki/Cron#Overview) syntax:

```bash
$ mosquitto_pub -u user -p password -t automation-service/set \
  -m "{\"schedule\": \"0 6 * * *\", \"job\": {\"start\": \"frontDoor\"}, \"jobId\": \"frontDoorJob\"}"
```

You can unschedule automation jobs with the jobId via "unschedule:"

```bash
$ mosquitto_pub -u user -p password -t automation-service/set m "{\"unschedule\": \"frontDoorJob\"}"
```

If you use Home Assistant, you can send messages to MQTT topics via its developer console as well. These messages and more can also be sent by scripts, so scripts can start and schedule other scripts. See an example of this in [/lua-automations/sunsetLighting.lua](lua-automations/sunsetLighting.lua).

Check out other examples in the [/lua-automations/](/lua-automations/) directory, and see the current Lua API reference documentation [here](docs/lua_api.md).


## Configuration

automation-service itself is configured in the [config.dhall](/config.dhall) file.

The `mqttBroker`'s `uri` setting determines how automation-service is going to connect to the network's broker. See the default config for an example of connecting via SSL using a password. 

If you don't want to configure SSL set caCertPath, clientCertPath, and clientKeyPath to `None`.

### Home Assistant Custom Card

The Custom Card receives data from automation-service as a sensor (honestly not sure if this is hacky or the right way to do it lol). The configuration for this is at [homeassistant/config/configuration.yaml](homeassistant/config/configuration.yaml), and the custom card itself is at [homeassistant/config/www/automation-service-card.js](homeassistant/config/www/automation-service-card.js), which is where you want to put it in your own system, and load it as a resource to enable it (p.s. I am a super newbie at writing HA code, so I'd appreciate any pointers on what I may be doing wrong here).


## Running

Currently the automation-service docker image is built on every push to master, and [published to the Github container registry](https://github.com/ddellacosta/automation-service/pkgs/container/automation-service). You can run this in a docker-compose stack a la https://www.home-assistant.io/installation/linux#install-home-assistant-container:

```yaml
  automation-service:
    container_name: automation-service
    image: ghcr.io/ddellacosta/automation-service:latest
    restart: unless-stopped
    networks:
      - localnet
    volumes:
      - ./automation-service/:/app
    environment:
      - TZ=America/New_York
```


## Development

### Building locally

This requires [nix](https://nixos.org/download.html).

```bash
$ nix build; docker load < result
...
Loaded image: automation-service:78921vslcfdribi4a6wyqx1cnl0nv67x
$

```

You can now take this and drop it in your `compose.yml` file for docker-compose.


### Testing

Running tests:

```bash
# watchexec is handy:
$ watchexec -w test -w src -w app -i "*.db" -i "test/dbs/*" 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```


## TODO

* bug: trying to restart an automation blocks when listening on channel, have to send an interrupt message somehow
* ensure scheduled automations are re-scheduled on restart
* better error handling and logging
* more, better documentation
* add (more) HTTP client functionality to Lua API
* improvements to HA dashboard integration
  * provide users with list of run-able automations somehow
  * ability to send messages
  * links to zigbee2mqtt for devices and groups
  * general UI/UX cleanup
* provide Lua API for lighting DSL (`Service.MQTT.Messages.Lighting`)
* should be able to record a scene easily based on a given group or arbitrarily specified set of devices. This could be an automation itself which just runs listening for the command to set scenes for any given room
* auto-import scenes for devices
* profile and better understand memory usage - seems like there is a very slow memory leak
* CI build and deploy: tag build with sha vs. everything going to 'latest' (?)
* move these todos into GH issues (?)
* figure out how to generate Lua docs rather than writing them by hand


## License/Copyright

GNU AGPLv3 License. Copyright © 2022-2023 Dave Della Costa, all rights otherwise reserved
