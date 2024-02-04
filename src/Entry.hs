module Entry (Entry (..), isActive, duration) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime, diffUTCTime)

data Entry = Entry
  { id :: Int,
    start :: UTCTime,
    end :: Maybe UTCTime
  }
  deriving (Show)

isActive :: Entry -> Bool
isActive = isNothing . end

duration :: UTCTime -> Entry -> NominalDiffTime
duration now entry = diffUTCTime end' (start entry)
  where
    end' = fromMaybe now (end entry)
