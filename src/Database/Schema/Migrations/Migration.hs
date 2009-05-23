module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration

    , MonadMigration(..)
    , MigrationMap
    )
where

import Database.Schema.Migrations.Dependencies

import qualified Data.Map as Map
import Data.Time () -- for UTCTime Show instance
import qualified Data.Time.Clock as Clock

data Migration = Migration { mTimestamp :: Clock.UTCTime
                           , mId :: String
                           , mDesc :: Maybe String
                           , mApply :: String
                           , mRevert :: Maybe String
                           , mDeps :: [String]
                           }
               deriving (Eq, Show, Ord)

type MigrationMap = Map.Map String Migration

instance Dependable Migration where
    depsOf = mDeps
    depId = mId

class (Monad m) => MonadMigration m where
    getCurrentTime :: m Clock.UTCTime

instance MonadMigration IO where
    getCurrentTime = Clock.getCurrentTime

newMigration :: (MonadMigration m) => String -> m Migration
newMigration theId = do
  curTime <- getCurrentTime
  return $ Migration { mTimestamp = curTime
                     , mId = theId
                     , mDesc = Nothing
                     , mApply = ""
                     , mRevert = Nothing
                     , mDeps = []
                     }