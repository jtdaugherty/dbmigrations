module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    , emptyMigration
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

emptyMigration :: String -> Migration
emptyMigration name =
  Migration { mTimestamp = Nothing
            , mId = name
            , mApply = ""
            , mRevert = Nothing
            , mDesc = Nothing
            , mDeps = []
            }

newMigration :: String -> Migration
newMigration theId = 
  (emptyMigration theId) 
    { mApply = "(Apply SQL here.)"
    , mDesc = Just "(Describe migration here.)"
    }
