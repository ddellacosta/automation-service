module Main
  ( integration
  , main
  , unit
  )
where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.Hspec (TreatPendingAs(..), testSpec)
import qualified Test.Integration.Service.Daemon as Daemon
import qualified Test.Unit.Service.DateHelpers as DateHelpers
import qualified Test.Unit.Service.Device as Devices
import qualified Test.Unit.Service.Group as Groups
import qualified Test.Unit.Service.MQTT.Status as MQTTStatus
import qualified Test.Unit.Service.MQTT.Messages.Daemon as Daemon.Messages

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
  dateHelpersSpec <- testSpec "DateHelpers Spec" DateHelpers.spec
  devicesSpec <- testSpec "Devices Spec" Devices.spec
  groupsSpec <- testSpec "Groups Spec" Groups.spec
  mqttStatusSpec <- testSpec "MQTT Status Messages Spec" MQTTStatus.spec
  pure $ testGroup "Unit Tests"
    [ automationMessagesSpec
    , dateHelpersSpec
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
