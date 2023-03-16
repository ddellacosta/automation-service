module Main
  ( integration
  , main
  , unit
  )
where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.Hspec (TreatPendingAs(..), testSpec)
import qualified Test.Integration.Service.Daemon as Daemon
import qualified Test.Unit.Service.Helpers as Helpers
import qualified Test.Unit.Service.Device as Devices
import qualified Test.Unit.Service.Messages.Daemon as Daemon.Messages

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
  appHelpersSpec <- testSpec "Helpers Spec" Helpers.spec
  devicesSpec <- testSpec "Devices Spec" Devices.spec
  pure $ testGroup "Unit Tests"
    [ automationMessagesSpec
    , appHelpersSpec
    , devicesSpec
    ]

integration :: IO TestTree
integration =
  localOption timeout' <$>
  localOption TreatPendingAsSuccess <$> do
    daemonSpec <- testSpec "Service.App.Daemon specs" Daemon.spec
    pure $ testGroup "Integration Tests" [ daemonSpec ]
    where
      timeout' = mkTimeout . (micros *) $ timeout
      micros = 1000000
