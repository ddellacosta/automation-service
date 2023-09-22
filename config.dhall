{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let LogLevel = ./config/LogLevel.dhall

in    { mqttBroker =
        { uri =
            "mqtts://automation-service:wvwSWIXIKnpiI6qyR8Z7ojg9BH4RWPbHY6SHTGvLWd2AeYPifH@mosquitto:8883"
        , automationServiceTopic = "automation-service/set"
        , caCertPath = Some "ca.crt"
        , clientCertPath = Some "client.crt"
        , clientKeyPath = Some "client.key"
        }
      , logFilePath = "logs/logfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "lua-automations/"
      , dbPath = "automationState.db"
      , httpPort = 8080
      }
    : ./config/Config.dhall
