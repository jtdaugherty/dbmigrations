{-# LANGUAGE OverloadedStrings #-}
module Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    , emptyMigration
    )
where

import Database.Schema.Migrations.Dependencies

import Data.Text ( Text )
import Data.Time () -- for UTCTime Show instance
import qualified Data.Time.Clock as Clock

data Migration = Migration { mTimestamp :: Maybe Clock.UTCTime
                           , mId :: Text
                           , mDesc :: Maybe Text
                           , mApply :: Text
                           , mRevert :: Maybe Text
                           , mDeps :: [Text]
                           }
               deriving (Eq, Show, Ord)

instance Dependable Migration where
    depsOf = mDeps
    depId = mId

emptyMigration :: Text -> Migration
emptyMigration name =
  Migration { mTimestamp = Nothing
            , mId = name
            , mApply = ""
            , mRevert = Nothing
            , mDesc = Nothing
            , mDeps = []
            }

newMigration :: Text -> Migration
newMigration theId = 
  (emptyMigration theId) 
    { mApply = "(Apply SQL here.)"
    , mDesc = Just "(Describe migration here.)"
    }
