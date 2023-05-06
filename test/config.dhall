{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

Note that dbPath must be post-processed by the test scaffolding in order to avoid conflicting db paths when tests are run in parallel.

-}
let LogLevel = ../config/LogLevel.dhall

in    { mqttBroker =
        { uri = "mqtt://localhost:1883"
        , automationServiceTopic = "automation-service/set"
        , statusTopic = "automation-service/status"
        , caCertPath = None Text
        , clientCertPath = None Text
        , clientKeyPath = None Text
        }
      , logFilePath = "logs/testlogfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "test/lua-automations/"
      , dbPath = "test/dbs/automationState"
      , cleaningLoopDelay = +30000
      }
    : ../config/Config.dhall
