# automation-service


Right now this software is usable, but in a very alpha state. See TODO below.


## Running

This requires [nix](https://nixos.org/download.html). It is only used in one command, but makes it easy to ensure builds are consistent, including dependencies. I will try to start providing docker images for direct download soon.

```bash
$ nix build; docker load < result
...
Loaded image: automation-service:78921vslcfdribi4a6wyqx1cnl0nv67x
$

```

You can take this now and drop it in your `compose.yml` file for docker-compose:

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


## Configuring automation-service

The `mqttBroker`'s `uri` setting determines how automation-service is going to connect to the network's broker. See the default config for an example of connecting with a password via SSL. 

If you don't want to configure SSL set caCertPath, clientCertPath, and clientKeyPath to `None`.


## TODO

* need to be able to shut off or edit scheduled automations
* auto-restarts of automations and reloading of automations when automation-service is restarted
* documentation
* better error handling
* query-able data about running and scheduled automations
* HA dashboard integration


## Testing

Running tests:

```bash
$ watchexec -w test -w src -w app 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```


## License/Copyright

GNU AGPLv3 License. Copyright © 2022-2023 Dave Della Costa, all rights otherwise reserved
