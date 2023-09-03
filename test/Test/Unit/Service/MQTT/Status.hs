module Test.Unit.Service.MQTT.Status
  ( spec
  ,
  )
where

import Control.Concurrent (myThreadId)
import Control.Lens (_Just, filtered, folded, lengthOf, toListOf, (^..))
import Data.Aeson (Value (..), decode)
import Data.Aeson.Lens (_Array, _String, key, nth, values)
import qualified Data.HashMap.Strict as M
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Service.Automation (Automation (..))
import Service.AutomationName (AutomationName (LuaScript))
import Service.Env (ThreadMap)
import qualified Service.MQTT.Messages.Daemon as Daemon
import Service.MQTT.Status (encodeAutomationStatus)
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO.Async (async)

luaAutomation :: UTCTime -> FilePath -> Automation IO
luaAutomation ts name = Automation
  { _name = LuaScript name
  , _cleanup = const $ pure ()
  , _run = const $ pure ()
  , _startTime = ts
  }

spec :: Spec
spec = describe "Status message generation" $ do
  it "generates Status message from running and scheduled automations, and device/group data" $ do
    startTime <- getCurrentTime
    dummyAsyncFix <- async $ pure ()
    placeholderThreadId <- myThreadId

    let
      luaAutomation' = luaAutomation startTime
      running = M.fromList
        [ (LuaScript "foo", (luaAutomation' "foo", dummyAsyncFix))
        , (LuaScript "bar", (luaAutomation' "bar", dummyAsyncFix))
        , (LuaScript "baz", (luaAutomation' "baz", dummyAsyncFix))
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

    -- first device in the list of devices --order doesn't matter here
    -- since all of the fixture automations should have 2 devices
    lengthOf (_Just . key "runningAutomations" . nth 0 . key "devices" . values) mStatus
      `shouldBe`
      2

    lengthOf
      (_Just . key "runningAutomations" . values . key "devices" . values . filtered (== String "device2"))
      mStatus
      `shouldBe`
      2

    (sort . toListOf (_Just . key "runningAutomations" . values . key "groups" . values) $ mStatus)
      `shouldBe`
      [ Number 1.0, Number 2.0 ]

    mStatus ^.. (_Just . key "runningAutomations" . _Array . folded . key "startTime" . _String)
      `shouldBe`
      (take 3 . repeat . (T.pack . iso8601Show) $ startTime)

    sort (mStatus ^.. _Just . key "scheduledAutomations" . _Array . folded . key "jobId" . _String)
      `shouldBe`
      ["job1", "job2", "job3"]

    sort (mStatus ^.. _Just . key "scheduledAutomations" . _Array . folded . key "schedule" . _String)
      `shouldBe`
      [ "0 12 * * *"
      , "0 22 * * *"
      , "0 5 * * *"
      ]
