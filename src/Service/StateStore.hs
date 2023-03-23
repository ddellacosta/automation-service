module Service.StateStore
  ( allRunning
  , updateRunning
  )
  where

import Data.Text (Text)
import Data.Foldable (for_)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (Connection)

allRunning :: FilePath -> IO [(Int, Text)]
allRunning dbPath = do
  dbConn <- DB.open dbPath
  createIfNotExists dbConn
  DB.query_ dbConn "SELECT * FROM running"

updateRunning :: FilePath -> [Text] -> IO ()
updateRunning dbPath runningAutomations = do
  dbConn <- DB.open dbPath
  -- this should be configurable somehow, and should dump to debug log entries
  -- DB.setTrace dbConn $ Just $ \t -> print t
  createIfNotExists dbConn
  DB.execute_ dbConn "DELETE FROM running"
  for_ runningAutomations $ \auto ->
    DB.execute dbConn "INSERT INTO running (automationName) VALUES (?)" [auto]

createIfNotExists :: Connection -> IO ()
createIfNotExists dbConn =
  DB.execute_ dbConn
    "CREATE TABLE IF NOT EXISTS running (id INTEGER PRIMARY KEY, automationName TEXT) STRICT"
