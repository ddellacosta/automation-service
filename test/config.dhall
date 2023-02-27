{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let LogLevel = ../config/LogLevel.dhall

in    { mqttBroker =
        { uri = "mqtt://localhost:1883"
        , automationServiceTopic = "automation-service/set"
        , caCertPath = None Text
        , clientCertPath = None Text
        , clientKeyPath = None Text
        }
      , logFilePath = "logs/testlogfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "test/lua-automations/"
      }
    : ../config/Config.dhall
