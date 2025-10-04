module Main
  ( integration
  , main
  , unit
  )
where

import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import qualified Options.Applicative as OptApp
import System.Environment (getArgs)
import qualified Test.Integration.Service.Daemon as Daemon
import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, includingOptions, localOption, mkTimeout, testGroup)
import Test.Tasty.Hspec (TreatPendingAs (..), testSpec)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Options (OptionDescription(..), IsOption(..))
import qualified Test.Tasty.Runners.Reporter as CIReporter
import qualified Test.Unit.Service.Device as Devices
import qualified Test.Unit.Service.Group as Groups
import qualified Test.Unit.Service.MQTT.Messages.Daemon as Daemon.Messages
import qualified Test.Unit.Service.MQTT.Status as MQTTStatus
import qualified Test.Unit.Service.TimeHelpers as TimeHelpers


timeout :: Integer
timeout = 10

newtype ReporterOpt = ReporterOpt String
  deriving (Eq, Ord, Typeable, Show)

instance IsOption ReporterOpt where
  defaultValue = ReporterOpt "ci"
  parseValue = \case
    "local" -> Just (ReporterOpt "local")
    _ -> Just defaultValue
  optionName = pure "reporter"
  optionHelp =
    pure "Reporter type: local or CI. Default is CI, which produces XML output for feeding into Allure Report. The 'local' option enables the default Tasty consoleTestReporter which is nicer to read when you're writing tests in your local environment."

reporterParser :: OptApp.Parser ReporterOpt
reporterParser = optionCLParser

parseReporter :: [String] -> IO ReporterOpt
parseReporter = OptApp.handleParseResult .
  (OptApp.execParserPure
    OptApp.defaultPrefs
    (OptApp.info reporterParser OptApp.fullDesc))

main :: IO ()
main = do
  reporterArgs <- getArgs <&>
    filter (\arg -> "--reporter" `isPrefixOf` arg)

  reporter :: ReporterOpt <-
    if null reporterArgs then
      pure defaultValue
    else
      parseReporter reporterArgs

  case reporter of
    ReporterOpt "local" ->
      defaultMainWithIngredients defaultIngredientsLocal' =<< allTests
    _ ->
      defaultMainWithIngredients defaultIngredientsCI' =<< allTests

  where
    reporterOptIngredient = includingOptions [Option (Proxy :: Proxy ReporterOpt)]
    defaultIngredientsCI' = CIReporter.ingredient : reporterOptIngredient : defaultIngredients
    defaultIngredientsLocal' = consoleTestReporter : reporterOptIngredient : defaultIngredients

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
