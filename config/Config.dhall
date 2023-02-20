{ mqttBroker : ./BrokerConfig.dhall
, devices : List ./DeviceConfig.dhall
, logFilePath : Text
, logLevel : ./LogLevel.dhall
, luaScriptPath : Text
}
