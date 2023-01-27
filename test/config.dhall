{-

Comments other than here will be stripped out because of how the Dhall auto-formatter works.

This config should not be checked into git with passwords and other sensitive values saved.

-}
let BrokerConfig
    : Type
    = { uri : Text
      , actionsServiceTopic : Text
      , caCertPath : Optional Text
      , clientCertPath : Optional Text
      , clientKeyPath : Optional Text
      }

let DeviceConfig
    : Type
    = { id : Text, name : Text, topic : Text }

let LogLevel
    : Type
    = < Debug | Info | Warn | Error >

let Config
    : Type
    = { mqttBroker : BrokerConfig
      , devices : List DeviceConfig
      , logFilePath : Text
      , logLevel : Text
      }

in  { mqttBroker =
      { uri = "mqtt://localhost:1883"
      , actionsServiceTopic = "actions-service/set"
      , caCertPath = None Text
      , clientCertPath = None Text
      , clientKeyPath = None Text
      }
    , devices = [] : List DeviceConfig
    , logFilePath = "logs/logfile"
    , logLevel = LogLevel.Debug
    }
