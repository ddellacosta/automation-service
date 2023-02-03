module Main
  ( integration
  , main
  , unit
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import qualified Test.Integration.Service.App.Daemon as Daemon
import qualified Test.Unit.Service.Device as Device
import qualified Test.Unit.Service.Messages.Action as Messages.Action


main :: IO ()
-- TODO make this configurable somehow--tasty filtering/args?
main = defaultMain =<< unit
-- main = defaultMain =<< allTests

-- allTests :: IO TestTree
-- allTests = do
--   unitTT <- unit
--   integrationTT <- integration
--   pure $ testGroup "All tests" [ unitTT, integrationTT ]

unit :: IO TestTree
unit = do
  deviceSpec <- testSpec "Device Spec" Device.spec_
  actionMessagesSpec <- testSpec "Messages.Action Spec" Messages.Action.spec_
  pure $ testGroup "Unit Tests" [ deviceSpec, actionMessagesSpec ]

integration :: IO TestTree
integration = do
  daemonSpec <- testSpec "Daemon Spec" Daemon.spec_
  pure $ testGroup "Integration Tests" [ daemonSpec ]
