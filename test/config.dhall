{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

dbPath here is unused by the integration tests: the scaffolding replaces it with a unique temporary-directory path per test invocation (see Test.Integration.Service.DaemonTestHelpers), which is deleted afterwards unless AUTOMATION_TEST_KEEP_DB is set.

-}
let LogLevel = ../config/lib/LogLevel.dhall

in    { mqttBroker =
        { uri = "mqtt://localhost:1883"
        , automationServiceTopic = "automation-service"
        , statusTopic = "automation-service/status"
        , caCertPath = None Text
        , clientCertPath = None Text
        , clientKeyPath = None Text
        }
      , logFilePath = "logs/testlogfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "test/lua-automations/"
      , dbPath = "test/dbs/automationState"
      , httpPort = 48080
      , httpRoot = "./ui/"
      }
    : ../config/lib/Config.dhall
