module Main
  ( integration
  , main
  , mainIntegration
  , unit
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import qualified Test.Integration.Service.App.Daemon as Daemon
import qualified Test.Unit.Service.App.Helpers as App.Helpers
import qualified Test.Unit.Service.App.DaemonState as App.DaemonState
import qualified Test.Unit.Service.Device as Device
import qualified Test.Unit.Service.Messages.Action as Messages.Action


main :: IO ()
-- TODO make this configurable somehow--tasty filtering/args?
main = defaultMain =<< allTests

allTests :: IO TestTree
allTests = unit >>= \unit' ->
  integration >>= \integration' ->
    pure $ testGroup "All Tests" [ unit', integration' ]

mainIntegration :: IO ()
mainIntegration = defaultMain =<< integration

unit :: IO TestTree
unit = do
  deviceSpec <- testSpec "Device Spec" Device.spec_
  actionMessagesSpec <- testSpec "Messages.Action Spec" Messages.Action.spec_
  appHelpersSpec <- testSpec "App.Helpers Spec" App.Helpers.spec
  appDaemonStateSpec <- testSpec "App.DaemonState Spec" App.DaemonState.spec
  pure $ testGroup "Unit Tests" [ deviceSpec, actionMessagesSpec, appHelpersSpec, appDaemonStateSpec ]

integration :: IO TestTree
integration = do
  daemonSpec <- testSpec "Daemon Spec" Daemon.spec_
  pure $ testGroup "Integration Tests" [ daemonSpec ]
