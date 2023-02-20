{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let LogLevel = ./config/LogLevel.dhall

in    { mqttBroker =
        { uri =
            "mqtts://actions-service:wvwSWIXIKnpiI6qyR8Z7ojg9BH4RWPbHY6SHTGvLWd2AeYPifH@mosquitto:8883"
        , actionsServiceTopic = "actions-service/set"
        , caCertPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/ca.crt"
        , clientCertPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/client.crt"
        , clientKeyPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/client.key"
        }
      , devices =
        [ { id = "GledoptoGLC007P_1"
          , name = "Gledopto GL-C-007P RGBW LED Controller Pro"
          , topic = "zigbee2mqtt/Gledopto GL-C-007P RGBW LED Controller Pro/set"
          }
        ]
      , logFilePath = "logs/logfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath =
          "/home/dd/code/home-assistant/actions-service/lua-actions/"
      }
    : ./config/Config.dhall
