{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Control.Applicative ()
import Database.SQLite.Simple (Connection)
import qualified Db as DB
import Entry
import System.Console.CmdArgs ( Data, Typeable, cmdArgs, modes )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

dbName :: String
dbName = "entries.sqlite"

ifTrackingOrElse :: (Connection -> IO ()) -> (Connection -> IO ()) -> IO ()
ifTrackingOrElse ifTracking ifNotTracking = DB.onDb dbName $ \db -> do
  last_ <- DB.lastEntry db
  case last_ of
    Just (Entry {end = Nothing}) -> ifTracking db
    _ -> ifNotTracking db

startTracking :: IO ()
startTracking =
  ifTrackingOrElse
    ( const $ do
        hPutStrLn stderr "Already running"
        exitFailure
    )
    ( \db -> do
        DB.addEntry db
        putStrLn "◉ now tracking"
    )

stopTracking :: IO ()
stopTracking =
  ifTrackingOrElse
    ( \db -> do
        DB.stopAll db
        putStrLn "○ stopped tracking"
    )
    ( const $ do
        hPutStrLn stderr "Not running"
        exitFailure
    )

-- data Interval = Day | Week | Month deriving (Read, Show, Data)
--
data CmdArgs
  = Start
  | Stop
  | Day
  | Week
  | Month
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  print =<< cmdArgs (modes [Start, Stop, Day, Week, Month])
