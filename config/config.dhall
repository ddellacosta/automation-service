{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let LogLevel = ./lib/LogLevel.dhall

in    { mqttBroker =
        { uri = "mqtt://mosquitto:1883"
        , automationServiceTopic = "automation-service/set"
        , statusTopic = "automation-service/status"
        , caCertPath = None Text
        , clientCertPath = None Text
        , clientKeyPath = None Text
        }
      , logFilePath = "logs/logfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "lua-automations/"
      , dbPath = "automationState.db"
      , httpPort = 8080
      }
    : ./lib/Config.dhall
