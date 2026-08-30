module Test.Unit.Service.StateStore
  ( spec
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, replicateM_, sequence_, void, when)
import System.Directory (listDirectory)
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (readSymbolicLink)
import qualified Service.StateStore as StateStore
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Service.StateStore" $ do

  it "closes database connections after each operation" $
    withSystemTempDirectory "statestore-test" $ \tmpDir -> do
      let
        dbPath = tmpDir ++ "/automationState.db"

        -- All four operations, once each.
        operations =
          [ StateStore.updateRunning dbPath ["someAutomation"]
          , StateStore.updateScheduled dbPath ["someMessage"]
          , void (StateStore.allRunning dbPath)
          , void (StateStore.allScheduled dbPath)
          ]

      -- Warm up: create the tables, so the measured operations run
      -- in the steady state.
      sequence_ operations

      -- Regression test for the connection leak fixed in 9399625:
      -- before the fix, every StateStore operation leaked an open
      -- SQLite connection (and its file descriptor), so the count of
      -- open handles grew monotonically forever. Counting handles
      -- that point at this exact db file makes the leak observable
      -- while remaining immune to unrelated fd noise from the test
      -- process.
      --
      -- /proc/self/fd is Linux-only, so the leak assertion is skipped
      -- on other platforms (the operations above still run and must
      -- not throw).
      when (os == "linux") $ do
        before <- countOpenHandlesTo dbPath
        replicateM_ 50 (sequence_ operations)
        after <- countOpenHandlesTo dbPath
        after `shouldBe` before

-- | Count open file descriptors pointing at the given path, by
-- reading the symlink targets in @/proc/self/fd@ (Linux only).
countOpenHandlesTo :: FilePath -> IO Int
countOpenHandlesTo path = do
  fds <- listDirectory "/proc/self/fd"
  length <$> filterM (pointsAt path) fds
  where
    pointsAt path' fd = do
      result <- try (readSymbolicLink ("/proc/self/fd/" ++ fd))
      pure $ case (result :: Either IOException FilePath) of
        Right target -> target == path'
        Left _ -> False