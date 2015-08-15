module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
where

import Database.Schema.Migrations.Dependencies

import Data.Time () -- for UTCTime Show instance
import qualified Data.Time.Clock as Clock

data Migration = Migration { mTimestamp :: Maybe Clock.UTCTime
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

newMigration :: String -> Migration
newMigration theId =
  Migration { mTimestamp = Nothing
            , mId = theId
            , mApply = "(Apply SQL here.)"
            , mRevert = Nothing
            , mDesc = Nothing
            , mDeps = []
            }
