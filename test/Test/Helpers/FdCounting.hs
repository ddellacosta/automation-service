module Test.Helpers.FdCounting
  ( countDbHandles
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import System.Directory (canonicalizePath, listDirectory)
import System.Info (os)
import System.Posix.Files (readSymbolicLink)

-- | Count open file descriptors pointing at the given path.
--
-- Linux reads symlink targets in @/proc/self/fd@; macOS iterates
-- fds and calls @fcntl(fd, F_GETPATH, buf)@ to get each one's
-- associated path. Returns -1 on unsupported platforms (the caller
-- guards on the OS and skips the assertion).
countDbHandles :: FilePath -> IO Int
countDbHandles dbPath' = case os of
  "linux"  -> countViaProc dbPath'
  "darwin" -> countViaMacOS dbPath'
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

-- | macOS: iterate fds 0..1023 and call @fcntl(fd, F_GETPATH, buf)@
-- to get each one's path. F_GETPATH is defined in @<fcntl.h>@ on
-- macOS (value 50); it writes the path of the file associated with
-- the fd into a caller-supplied buffer. Returns -1 on non-open or
-- non-file fds (sockets, pipes, etc.), which we skip.
--
-- macOS resolves symlinks in filesystem paths (@/var@ is a symlink
-- to @\/private\/var@), and F_GETPATH returns the resolved path —
-- so we canonicalize the input path before comparing.
countViaMacOS :: FilePath -> IO Int
countViaMacOS dbPath' = do
  canonicalPath <- canonicalizePath dbPath'
  results <- mapM (checkFd canonicalPath) [0 .. 1023]
  pure . length . catMaybes $ results
  where
    checkFd path' fdNum = do
      mPath <- getFdPathMac (fromIntegral fdNum)
      pure $ case mPath of
        Just p | p == path' -> Just ()
        _                   -> Nothing

    getFdPathMac :: CInt -> IO (Maybe FilePath)
    getFdPathMac fdNum =
      allocaBytes 4096 $ \(buf :: CString) -> do
        r <- c_fcntl_path fdNum f_GETPATH buf
        if r == -1
          then pure Nothing
          else Just <$> peekCString buf

-- | F_GETPATH from @<fcntl.h>@ on macOS.
f_GETPATH :: CInt
f_GETPATH = 50

-- | Direct FFI call to @fcntl@. fcntl is variadic in C, but the GHC
-- FFI can call it with the specific argument types we use (fd:
-- CInt, cmd: CInt, buf: CString = Ptr CChar). GHC2021 includes
-- ForeignFunctionInterface, so no additional pragma is needed.
--
-- CInt(..) must be imported (not just CInt) because FFI marshalling
-- requires the newtype's data constructor to be in scope.
foreign import ccall "fcntl"
  c_fcntl_path :: CInt -> CInt -> CString -> IO CInt