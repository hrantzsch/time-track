{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Db (onDb, dumpEntries, lastEntry, addEntry, stopAll, create, today, sinceDate, secondsToday, secondsThisWeek, secondsThisMonth) where

import Data.Maybe
import Data.Time.Calendar (Day)
import Database.SQLite.Simple
  ( Connection,
    FromRow,
    Only (..),
    execute_,
    field,
    fromRow,
    query,
    query_,
    withConnection,
  )
import Entry

type Seconds = Integer

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field

onDb :: String -> (Connection -> IO a) -> IO a
onDb dbName f = withConnection dbName $ \conn -> f conn

create :: Connection -> IO ()
create conn =
  execute_
    conn
    "CREATE TABLE entries (\
    \id INTEGER PRIMARY KEY, \
    \start TEXT DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%S', 'now')), \
    \end TEXT DEFAULT NULL\
    \)"

dumpEntries :: Connection -> IO String
dumpEntries conn =
  show <$> (query_ conn "SELECT * FROM entries" :: IO [Entry])

lastEntry :: Connection -> IO (Maybe Entry)
lastEntry conn = listToMaybe <$> (query_ conn "SELECT * FROM entries ORDER BY id DESC LIMIT 1" :: IO [Entry])

addEntry :: Connection -> IO ()
addEntry conn = execute_ conn "INSERT INTO entries DEFAULT VALUES"

stopAll :: Connection -> IO ()
stopAll conn = execute_ conn "UPDATE entries SET end = DATETIME('now') WHERE end IS NULL"

today :: Connection -> IO [Entry]
today conn = query_ conn "SELECT * FROM entries WHERE DATE(start) = DATE('now')" :: IO [Entry]

secondsToday :: Connection -> IO Seconds
secondsToday conn = do
  [[seconds :: Seconds]] <-
    query_
      conn
      "SELECT SUM(UNIXEPOCH(end_) - UNIXEPOCH(start)) \
      \FROM (SELECT start, COALESCE(end, DATETIME('now')) AS end_ FROM entries) \
      \WHERE DATE(start) >= DATE('now')"
  pure seconds

secondsThisWeek :: Connection -> IO Seconds
secondsThisWeek conn = do
  [[seconds :: Seconds]] <-
    query_
      conn
      "SELECT SUM(UNIXEPOCH(end_) - UNIXEPOCH(start)) \
      \FROM (SELECT start, COALESCE(end, DATETIME('now')) AS end_ FROM entries) \
      \WHERE DATE(start) >= DATE('now', 'weekday 0', '-6 days')"
  pure seconds

secondsThisMonth :: Connection -> IO Seconds
secondsThisMonth conn = do
  [[seconds :: Seconds]] <-
    query_
      conn
      "SELECT SUM(UNIXEPOCH(end_) - UNIXEPOCH(start)) \
      \FROM (SELECT start, COALESCE(end, DATETIME('now')) AS end_ FROM entries) \
      \WHERE DATE(start) >= DATE('now', 'start of month')"
  pure seconds

-- | Fetch all entries from the database where the start day is after the given day
sinceDate :: Day -> Connection -> IO [Entry]
sinceDate sinceTime conn =
  query
    conn
    "SELECT * FROM entries WHERE DATE(start) >= DATE(?)"
    (Only sinceTime) ::
    IO [Entry]
