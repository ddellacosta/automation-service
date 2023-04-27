# automation-service

automation-service is a tool for setting up simple-to-complicated automations. It communicates via MQTT and can automatically pull and use device and group information from Zigbee2MQTT, but it can be used with anything that can communicate over MQTT. It provides a simple message protocol for starting, stopping, and scheduling automations. It provides the ability to script your automations in Lua with batteries-included helpers, or build them in Haskell for more complex or "infrastructural" requirements.

Right now this software is usable, but in a very alpha state. See [TODO](#todo) below.


## Configuration

The `mqttBroker`'s `uri` setting determines how automation-service is going to connect to the network's broker. See the default config for an example of connecting with a password via SSL. 

If you don't want to configure SSL set caCertPath, clientCertPath, and clientKeyPath to `None`.


## Running

Currently the automation-service docker image is built on every push to master and [published to the Github container registry](https://github.com/ddellacosta/automation-service/pkgs/container/automation-service). You can then run this in a docker-compose stack a la https://www.home-assistant.io/installation/linux#install-home-assistant-container:

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

You can then start this up with `docker-compose start automation-service`. Check out the automations in the `lua-automations` directory to get going. #TODO <- link here to examples


## TODO

* bug: an automation failing and shutting down or just not looping doesn't get removed from list of running automations
* bug: trying to restart an automation blocks when listening on channel, have to send an interrupt message somehow
* documentation
* add HTTP client functionality and add Lua API
* improvements to HA dashboard integration
  * provide users with list of run-able automations somehow
  * links to zigbee2mqtt for devices and groups
  * general UI/UX cleanup
* provide Lua API for lighting
* better error handling and logging
* should be able to record a scene easily based on a given group or arbitrarily specified set of devices. This could be an automation itself which just runs listening for the command to set scenes for any given room
* auto-import scenes for devices
* profile and better understand memory usage - seems like there is a very slow memory leak
* CI build and deploy: tag build with sha vs. everything going to 'latest' (?)


## Development

### Building locally

This requires [nix](https://nixos.org/download.html). It is only used in one command, but makes it easy to ensure builds are consistent, including dependencies.

```bash
$ nix build; docker load < result
...
Loaded image: automation-service:78921vslcfdribi4a6wyqx1cnl0nv67x
$

```

You can now take this and drop it in your `compose.yml` file for docker-compose:


### Testing

Running tests:

```bash
# watchexec is handy:
$ watchexec -w test -w src -w app -i "*.db" -i "test/dbs/*" 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```


## License/Copyright

GNU AGPLv3 License. Copyright © 2022-2023 Dave Della Costa, all rights otherwise reserved
