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
      { uri = "%%MQTT-URI%%"
      , actionsServiceTopic = "actions-service/set"
      , caCertPath = Some "/home/dd/code/home-assistant/mosquitto_config/ca.crt"
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
    }
