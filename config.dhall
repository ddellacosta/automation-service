{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let LogLevel = ./config/LogLevel.dhall

in    { mqttBroker =
        { uri =
            "mqtts://automation-service:wvwSWIXIKnpiI6qyR8Z7ojg9BH4RWPbHY6SHTGvLWd2AeYPifH@mosquitto:8883"
        , automationServiceTopic = "automation-service/set"
        , caCertPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/ca.crt"
        , clientCertPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/client.crt"
        , clientKeyPath = Some
            "/home/dd/code/home-assistant/mosquitto_config/client.key"
        }
      , logFilePath = "logs/logfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath =
          "/home/dd/code/home-assistant/automation-service/lua-automations/"
      }
    : ./config/Config.dhall
