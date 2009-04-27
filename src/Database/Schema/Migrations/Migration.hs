module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
where

import Database.Schema.Migrations.Dependencies

import System.Random ( Random(randomIO) )

import Data.Time () -- for UTCTime Show instance
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Time.Clock ( UTCTime, getCurrentTime )

data Migration = Migration { mTimestamp :: UTCTime
                           , mId :: String
                           , mDesc :: Maybe String
                           , mApply :: String
                           , mRevert :: Maybe String
                           , mDeps :: [String]
                           }
               deriving (Eq, Show, Ord)

instance Dependable Migration where
    depsOf = mDeps
    depId = mId

newMigration :: String -> IO Migration
newMigration theId = do
  curTime <- getCurrentTime
  return $ Migration { mTimestamp = curTime
                     , mId = theId
                     , mDesc = Nothing
                     , mApply = ""
                     , mRevert = Nothing
                     , mDeps = []
                     }