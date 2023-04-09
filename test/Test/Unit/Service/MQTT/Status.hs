module Test.Unit.Service.MQTT.Status
  ( spec
  ,
  )
where

import Control.Concurrent (myThreadId)
import Control.Lens ((^?), _Just, folded, lengthOf)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value, decode)
import Data.Aeson.Lens (key, nth, values)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as M
import Service.Automation (Automation(..))
import Service.AutomationName (AutomationName(LuaScript))
import Service.Env (ThreadMap)
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Status (encodeAutomationStatus)
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO.Async (async)

luaAutomation :: FilePath -> Automation IO
luaAutomation name = Automation
  { _name = LuaScript name
  , _cleanup = const $ pure ()
  , _run = const $ pure ()
  }

spec :: Spec
spec = describe "Status message generation" $ do
  it "generates Status message from running and scheduled automations, and device/group data" $ do
    dummyAsyncFix <- async $ pure ()
    placeholderThreadId <- myThreadId

    let
      running = M.fromList
        [ (LuaScript "foo", (luaAutomation "foo", dummyAsyncFix))
        , (LuaScript "bar", (luaAutomation "bar", dummyAsyncFix))
        , (LuaScript "baz", (luaAutomation "baz", dummyAsyncFix))
        ] :: ThreadMap IO

      --  Service.Env.ScheduledJobs
      scheduled = M.fromList
        [ ("job1", ("0 5 * * *", Daemon.Start (LuaScript "foo"), placeholderThreadId))
        , ("job2", ("0 22 * * *", Daemon.Stop (LuaScript "foo"), placeholderThreadId))
        , ("job3", ("0 12 * * *", Daemon.Stop (LuaScript "qux"), placeholderThreadId))
        ]

      -- Service.Env.Registrations Service.Device.DeviceId
      deviceRegs = M.fromList
        [ ("device1", LuaScript "baz" :| [LuaScript "foo"])
        , ("device2", LuaScript "baz" :| [LuaScript "bar"])
        , ("device3", LuaScript "bar" :| [LuaScript "foo"])
        ]

      --  Service.Env.Registrations Service.Group.GroupId
      groupRegs = M.fromList
        [ (1, LuaScript "foo" :| [])
        , (2, LuaScript "bar" :| [])
        ]

      statusJSON = encodeAutomationStatus running scheduled deviceRegs groupRegs
      mStatus = decode statusJSON :: Maybe Value

    (lengthOf (folded . key "runningAutomations" . values) mStatus) `shouldBe` 3
    (lengthOf (folded . key "scheduledAutomations" . values) mStatus) `shouldBe` 3

    -- first device in the list of devices for `LuaScript "baz"`
    lengthOf (_Just . key "runningAutomations" . nth 0 . key "devices" . values) mStatus
      `shouldBe`
      2

    mStatus ^? _Just . key "runningAutomations" . nth 0 . key "devices" . nth 0
      `shouldBe`
      Just (Aeson.String "device1")

    -- first group in the list of groups for `LuaScript "baz"`
    lengthOf (_Just . key "runningAutomations" . nth 1 . key "groups" . values) mStatus
      `shouldBe`
      1

    mStatus ^? _Just . key "runningAutomations" . nth 1 . key "groups" . nth 0
      `shouldBe`
      Just (Aeson.Number 1)