module Service.StateStore
  ( allRunning
  , allScheduled
  , updateRunning
  , updateScheduled
  )
  where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as DB
import UnliftIO.Exception (bracket)

-- | Runs an action against a connection to the database at the given
-- path, ensuring the connection is closed afterwards. All database
-- access in this module goes through this function so that
-- connections (and the file descriptors and memory they retain) can
-- never be leaked, even if the action throws or is interrupted by an
-- async exception (UnliftIO's bracket masks the cleanup handler,
-- guaranteeing the close runs).
--
withDBConn :: FilePath -> (Connection -> IO a) -> IO a
withDBConn dbPath =
  bracket acquire DB.close
  where
    acquire = do
      conn <- DB.open dbPath
      -- SQLite's default busy timeout is zero, so touching the
      -- database while another connection holds the lock fails
      -- immediately with SQLITE_BUSY. Waiting briefly instead keeps
      -- transient concurrent access from crashing the caller — an
      -- uncaught SQLError in StateManager would kill its thread and
      -- silently end state persistence, the same failure mode as
      -- the fd-exhaustion bug this bracket fixed. Exposed by the
      -- connection-leak regression test polling the running set
      -- while StateManager was mid-write.
      DB.execute_ conn "PRAGMA busy_timeout = 5000"
      pure conn

allRunning :: FilePath -> IO [(Int, Text)]
allRunning dbPath = withDBConn dbPath $ \dbConn -> do
  createRunningIfNotExists dbConn
  DB.query_ dbConn "SELECT * FROM running"

updateRunning :: FilePath -> [Text] -> IO ()
updateRunning dbPath runningAutomations = withDBConn dbPath $ \dbConn -> do
  -- this should be configurable somehow, and should dump to debug log entries
  -- DB.setTrace dbConn $ Just $ \t -> print t
  createRunningIfNotExists dbConn
  -- The transaction wraps the DELETE and all of the INSERTs so they
  -- are applied atomically, and so we only pay a single commit
  -- (fsync) instead of one per statement.
  DB.withTransaction dbConn $ do
    DB.execute_ dbConn "DELETE FROM running"
    DB.executeMany dbConn
      "INSERT INTO running (automationName) VALUES (?)"
      [[auto] | auto <- runningAutomations]

createRunningIfNotExists :: Connection -> IO ()
createRunningIfNotExists dbConn =
  DB.execute_ dbConn
    "CREATE TABLE IF NOT EXISTS running (id INTEGER PRIMARY KEY, automationName TEXT) STRICT"

allScheduled :: FilePath -> IO [(Int, ByteString)]
allScheduled dbPath = withDBConn dbPath $ \dbConn -> do
  createScheduledIfNotExists dbConn
  DB.query_ dbConn "SELECT * FROM scheduled"

createScheduledIfNotExists :: Connection -> IO ()
createScheduledIfNotExists dbConn =
  DB.execute_ dbConn
    "CREATE TABLE IF NOT EXISTS scheduled (id INTEGER PRIMARY KEY, message BLOB) STRICT"

--
-- scheduledAutos is a list of ByteString-encoded Daemon.Message
-- values:
--
updateScheduled :: FilePath -> [ByteString] -> IO ()
updateScheduled dbPath scheduledAutos = withDBConn dbPath $ \dbConn -> do
  -- this should be configurable somehow, and should dump to debug log entries
  -- DB.setTrace dbConn $ Just $ \t -> print t
  createScheduledIfNotExists dbConn
  -- See note in updateRunning on the transaction/executeMany usage.
  DB.withTransaction dbConn $ do
    DB.execute_ dbConn "DELETE FROM scheduled"
    DB.executeMany dbConn
      "INSERT INTO scheduled (message) VALUES (?)"
      [[msg] | msg <- scheduledAutos]
