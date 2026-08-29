{-

Local A/B test config for the perf/leak harness. Matches the current
config format (post-PR #45); the lib/ directory in here must stay in
sync with the repo's config/lib/ (see README.md).

-}
let LogLevel = ./lib/LogLevel.dhall

in    { mqttBroker =
        { uri = "mqtt://mosquitto:1883"
        , automationServiceTopic = "automation-service"
        , statusTopic = "automation-service/status"
        , caCertPath = None Text
        , clientCertPath = None Text
        , clientKeyPath = None Text
        }
      , logFilePath = "logs/logfile"
      , logLevel = LogLevel.Debug
      , luaScriptPath = "lua-automations/"
      , dbPath = "data/automationState.db"
      , httpPort = 8080
      , httpRoot = "./ui/"
      }
    : ./lib/Config.dhall