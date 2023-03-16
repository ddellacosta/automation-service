# automation-service

automation-service is a tool for setting up simple-to-complicated automations. It communicates via MQTT and can automatically pull and use device information from Zigbee2MQTT (only, for now, ESPHome next). It provides a simple message protocol for starting, stopping, and scheduling automations. It provides the ability to script your automations in Lua with batteries-included helpers, or build them in Haskell for more complex tasks.

Right now this software is usable, but in a very alpha state. See [TODO](#todo) below.


## Configuration

The `mqttBroker`'s `uri` setting determines how automation-service is going to connect to the network's broker. See the default config for an example of connecting with a password via SSL. 

If you don't want to configure SSL set caCertPath, clientCertPath, and clientKeyPath to `None`.


## Running

This requires [nix](https://nixos.org/download.html). It is only used in one command, but makes it easy to ensure builds are consistent, including dependencies. I hope to offer pre-built docker images for direct download soon.

```bash
$ nix build; docker load < result
...
Loaded image: automation-service:78921vslcfdribi4a6wyqx1cnl0nv67x
$

```

You can now take this and drop it in your `compose.yml` file for docker-compose:

```yaml
  automation-service:
    container_name: automation-service
    image: automation-service:78921vslcfdribi4a6wyqx1cnl0nv67x
    restart: unless-stopped
    networks:
      - localnet
    volumes:
       - ./automation-service/:/app
    environment:
      - TZ=America/New_York
```

You can then start this up `docker-compose start automation-service`, and can drop automations in the `lua-automations` directory to test.


## TODO

* ability to auto-load groups and scenes and easily integrate in scripts
* need to be able to shut off or edit scheduled automations
* auto-restarts of automations and reloading of automations when automation-service is restarted
* generalize Gledopto Lighting related Message module and provide Lua API for it
* documentation
* better error handling
* query-able data about running and scheduled automations
* HA dashboard integration
* profile and better understand memory usage
* set up GH actions (?) to run build and store docker image somewhere, I guess not docker hub any more (https://blog.alexellis.io/docker-is-deleting-open-source-images/) ...[github packages](https://docs.github.com/en/actions/publishing-packages/publishing-docker-images#publishing-images-to-github-packages)?
* should be able to record a scene easily based on a given group or arbitrarily specified set of devices. This could be an automation itself which just runs listening for the command to set scenes for any given room
* auto-import scenes for devices


## Testing

Running tests:

```bash
$ watchexec -w test -w src -w app 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```


## License/Copyright

GNU AGPLv3 License. Copyright © 2022-2023 Dave Della Costa, all rights otherwise reserved
