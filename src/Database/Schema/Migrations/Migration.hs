module Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationID
    , newMigration
    )
where

import Database.Schema.Migrations.Dependencies

import System.Random ( Random(randomIO) )

import Data.Time () -- for UTCTime Show instance
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Time.Clock ( UTCTime, getCurrentTime )

type MigrationID = String

data Migration = Migration { mTimestamp :: UTCTime
                           , mId :: MigrationID
                           , mDesc :: Maybe String
                           , mApply :: String
                           , mRevert :: Maybe String
                           , mDeps :: [Migration]
                           }
               deriving (Eq, Show)

instance Dependable Migration where
    depsOf = mDeps

newMigration :: MigrationID -> IO Migration
newMigration theId = do
  curTime <- getCurrentTime
  return $ Migration { mTimestamp = curTime
                     , mId = theId
                     , mDesc = Nothing
                     , mApply = ""
                     , mRevert = Nothing
                     , mDeps = []
                     }