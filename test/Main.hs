module Main
  ( integration
  , main
  , unit
  )
where

import qualified Test.Integration.Service.Daemon as Daemon
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.Hspec (TreatPendingAs (..), testSpec)
import qualified Test.Unit.Service.Device as Devices
import qualified Test.Unit.Service.Group as Groups
import qualified Test.Unit.Service.MQTT.Messages.Daemon as Daemon.Messages
import qualified Test.Unit.Service.MQTT.Status as MQTTStatus
import qualified Test.Unit.Service.TimeHelpers as TimeHelpers

timeout :: Integer
timeout = 10

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
  timeHelpersSpec <- testSpec "TimeHelpers Spec" TimeHelpers.spec
  devicesSpec <- testSpec "Devices Spec" Devices.spec
  groupsSpec <- testSpec "Groups Spec" Groups.spec
  mqttStatusSpec <- testSpec "MQTT Status Messages Spec" MQTTStatus.spec
  pure $ testGroup "Unit Tests"
    [ automationMessagesSpec
    , timeHelpersSpec
    , devicesSpec
    , groupsSpec
    , mqttStatusSpec
    ]

integration :: IO TestTree
integration =
  localOption timeout' <$>
  localOption TreatPendingAsSuccess <$> do
    daemonSpec <- testSpec "Service.Daemon specs" Daemon.spec
    pure $ testGroup "Integration Tests" [ daemonSpec ]
    where
      timeout' = mkTimeout . (micros *) $ timeout
      micros = 1000000
