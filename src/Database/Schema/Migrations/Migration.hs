module Database.Schema.Migrations.Migration
    ( Migration(..)
    , MigrationID
    , newMigration
    )
where

import Database.Schema.Migrations.Dependencies

import System.Random ( Random(randomIO) )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Time.Clock ( UTCTime, getCurrentTime )

type MigrationID = Int

data Migration = Migration { mTimestamp :: UTCTime
                           , mId :: MigrationID
                           , mDesc :: Maybe String
                           , mApply :: String
                           , mRevert :: String
                           , mDeps :: [Migration]
                           }
               deriving (Eq)

instance Dependable Migration where
    depsOf = mDeps

makeMigrationId :: IO MigrationID
makeMigrationId = randomIO

newMigration :: IO Migration
newMigration = do
  curTime <- getCurrentTime
  id <- makeMigrationId
  return $ Migration { mTimestamp = curTime
                     , mId = id
                     , mDesc = Nothing
                     , mApply = ""
                     , mRevert = ""
                     , mDeps = []
                     }