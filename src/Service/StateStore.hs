module Service.StateStore
  ( allRunning
  , allScheduled
  , updateRunning
  , updateScheduled
  )
  where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Foldable (for_)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (Connection)

allRunning :: FilePath -> IO [(Int, Text)]
allRunning dbPath = do
  dbConn <- DB.open dbPath
  createRunningIfNotExists dbConn
  DB.query_ dbConn "SELECT * FROM running"

updateRunning :: FilePath -> [Text] -> IO ()
updateRunning dbPath runningAutomations = do
  dbConn <- DB.open dbPath
  -- this should be configurable somehow, and should dump to debug log entries
  -- DB.setTrace dbConn $ Just $ \t -> print t
  createRunningIfNotExists dbConn
  DB.execute_ dbConn "DELETE FROM running"
  for_ runningAutomations $ \auto ->
    DB.execute dbConn "INSERT INTO running (automationName) VALUES (?)" [auto]

createRunningIfNotExists :: Connection -> IO ()
createRunningIfNotExists dbConn =
  DB.execute_ dbConn
    "CREATE TABLE IF NOT EXISTS running (id INTEGER PRIMARY KEY, automationName TEXT) STRICT"

allScheduled :: FilePath -> IO [(Int, ByteString)]
allScheduled dbPath = do
  dbConn <- DB.open dbPath
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
updateScheduled dbPath scheduledAutos = do
  dbConn <- DB.open dbPath
  -- this should be configurable somehow, and should dump to debug log entries
  -- DB.setTrace dbConn $ Just $ \t -> print t
  createScheduledIfNotExists dbConn
  DB.execute_ dbConn "DELETE FROM scheduled"
  for_ scheduledAutos $ \msg ->
    DB.execute dbConn "INSERT INTO scheduled (message) VALUES (?)" [msg]
