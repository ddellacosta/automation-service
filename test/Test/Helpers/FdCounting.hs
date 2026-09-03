module Test.Helpers.FdCounting
  ( countDbHandles
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import System.Directory (listDirectory)
import System.Info (os)
import System.Posix.Files (readSymbolicLink)

-- | Count open file descriptors pointing at the given path.
--
-- Linux reads symlink targets in @/proc/self/fd@. Returns -1 on
-- unsupported platforms (the caller guards on the OS and skips the
-- assertion).
countDbHandles :: FilePath -> IO Int
countDbHandles dbPath' = case os of
  "linux"  -> countViaProc dbPath'
  "darwin" -> pure (-1) -- TODO: untested; revisit when macOS testing is available
  _        -> pure (-1)

-- | Linux: read symlink targets in @/proc/self/fd@.
countViaProc :: FilePath -> IO Int
countViaProc dbPath' = do
  fds <- listDirectory "/proc/self/fd"
  length <$> filterM (pointsAt dbPath') fds
  where
    pointsAt path' fd = do
      result <- try (readSymbolicLink ("/proc/self/fd/" ++ fd))
      pure $ case (result :: Either IOException FilePath) of
        Right target -> target == path'
        Left _       -> False