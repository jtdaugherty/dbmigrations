module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
where

import Database.Schema.Migrations.Dependencies

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

instance Dependable Migration where
    depsOf = mDeps
    depId = mId

newMigration :: String -> IO Migration
newMigration theId = do
  curTime <- Clock.getCurrentTime
  return $ Migration { mTimestamp = curTime
                     , mId = theId
                     , mDesc = Nothing
                     , mApply = ""
                     , mRevert = Nothing
                     , mDeps = []
                     }
