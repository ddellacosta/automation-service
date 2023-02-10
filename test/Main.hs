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
import qualified Test.Unit.Service.Device as Device
import qualified Test.Unit.Service.Messages.Action as Messages.Action


main :: IO ()
main = defaultMain =<< allTests

allTests :: IO TestTree
allTests = do
  unit' <- unit
  integration' <- integration
  pure $ testGroup "All Tests" [ unit', integration' ]

unit :: IO TestTree
unit = do
  deviceSpec <- testSpec "Device Spec" Device.spec
  actionMessagesSpec <- testSpec "Messages.Action Spec" Messages.Action.spec
  appHelpersSpec <- testSpec "App.Helpers Spec" App.Helpers.spec
  appDaemonStateSpec <- testSpec "App.DaemonState Spec" App.DaemonState.spec
  pure $ testGroup "Unit Tests"
    [ actionMessagesSpec
    , appDaemonStateSpec
    , appHelpersSpec
    , deviceSpec
    ]

integration :: IO TestTree
integration = do
  daemonSpec <- testSpec "Daemon Spec" Daemon.spec
  pure $ testGroup "Integration Tests" [ daemonSpec ]
