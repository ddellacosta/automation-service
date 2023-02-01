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


main :: IO ()
main = defaultMain =<< unit

unit :: IO TestTree
unit = do
  deviceSpec <- testSpec "Device Spec" Device.spec_
  pure $ testGroup "Unit Tests" [ deviceSpec ]

integration :: IO TestTree
integration = do
  daemonSpec <- testSpec "Daemon Spec" Daemon.spec_
  pure $ testGroup "Integration Tests" [ daemonSpec ]
