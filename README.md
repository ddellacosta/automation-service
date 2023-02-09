# Actions Service

Actions Service provides a way to build complex flows using MQTT topics and HTTP endpoints as necessary. It runs as a service and is intended to be bundled in a docker-compose setup for use with Home Assistant and other home automation tools.

## Testing

Running tests:

```bash
$ watchexec -w test -w src -w app 'cabal test --test-show-details=always --test-options "--color=always"'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Unit'
$ cabal test --test-show-details=always --test-options '--color=always -l -p Integration'
```

## License/Copyright

MIT License, all rights otherwise reserved. Copyright © 2022-2023 Dave Della Costa
