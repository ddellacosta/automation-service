module Main
  ( integration
  , main
  , unit
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import qualified Test.Integration.Service.App.Daemon as Daemon
import qualified Test.Unit.Service.App.Helpers as App.Helpers
import qualified Test.Unit.Service.App.DaemonState as App.DaemonState
import qualified Test.Unit.Service.Messages.Daemon as Daemon.Messages
import qualified Test.Unit.Service.Messages.Zigbee2MQTTDevice as Zigbee2MQTTDevice.Messages


main :: IO ()
main = defaultMain =<< allTests

allTests :: IO TestTree
allTests = do
  unit' <- unit
  integration' <- integration
  pure $ testGroup "All Tests" [ unit', integration' ]

unit :: IO TestTree
unit = do
  automationMessagesSpec <- testSpec "Daemon.Messages Spec" Daemon.Messages.spec
  appDaemonStateSpec <- testSpec "App.DaemonState Spec" App.DaemonState.spec
  appHelpersSpec <- testSpec "App.Helpers Spec" App.Helpers.spec
  zigbee2mqttMessagesSpec <- testSpec "Zigbee2MQTTDevice.Messages Spec" Zigbee2MQTTDevice.Messages.spec
  pure $ testGroup "Unit Tests"
    [ automationMessagesSpec
    , appDaemonStateSpec
    , appHelpersSpec
    , zigbee2mqttMessagesSpec
    ]

integration :: IO TestTree
integration = do
  daemonSpec <- testSpec "Service.App.Daemon specs" Daemon.spec
  pure $ testGroup "Integration Tests" [ daemonSpec ]
