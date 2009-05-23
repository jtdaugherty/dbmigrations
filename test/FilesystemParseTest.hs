module FilesystemParseTest
    ( tests
    )
where

import Test.HUnit
import Data.Time.Clock ( UTCTime )
import System.FilePath ( (</>) )

import Common

import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Filesystem
    ( FilesystemStore(..)
    , migrationFromFile
    )

tests :: IO [Test]
tests = migrationParsingTests

-- filename, result
type MigrationParsingTestCase = (FilePath, Either String Migration)

tsStr :: String
tsStr = "2009-04-15 10:02:06 UTC"

ts :: UTCTime
ts = read tsStr

valid_full :: Migration
valid_full = Migration {
               mTimestamp = ts
             , mId = "valid_full"
             , mDesc = Just "A valid full migration."
             , mDeps = ["another_migration"]
             , mApply = "  CREATE TABLE test (\n    a int\n  );\n"
             , mRevert = Just "DROP TABLE test;"
             }

testStorePath :: FilePath
testStorePath = testFile $ "migration_parsing"

fp :: FilePath -> FilePath
fp = (testStorePath </>)

migrationParsingTestCases :: [MigrationParsingTestCase]
migrationParsingTestCases = [ ("valid_full", Right valid_full)
                            , ("valid_with_comments"
                              , Right (valid_full { mId = "valid_with_comments" }))
                            , ("valid_no_depends"
                              , Right (valid_full { mId = "valid_no_depends", mDeps = [] }))
                            , ("valid_no_desc"
                              , Right (valid_full { mId = "valid_no_desc", mDesc = Nothing }))
                            , ("valid_no_revert"
                              , Right (valid_full { mId = "valid_no_revert", mRevert = Nothing }))
                            , ("invalid_missing_required_fields"
                              , Left $ "Missing required field(s) in migration " ++
                                         (show $ fp "invalid_missing_required_fields") ++
                                         ": [\"Created\",\"Depends\"]")
                            , ("invalid_deps_list"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_deps_list"))
                            , ("invalid_field_name"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_field_name"))
                            , ("invalid_syntax"
                              , Left $ "Could not parse migration file " ++
                                         (show $ fp "invalid_syntax"))
                            , ("invalid_timestamp"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_timestamp"))
                            ]

mkParsingTest :: MigrationParsingTestCase -> IO Test
mkParsingTest (fname, expected) = do
  let store = FSStore { storePath = testStorePath }
  actual <- migrationFromFile store fname
  return $ test $ expected ~=? actual

migrationParsingTests :: IO [Test]
migrationParsingTests =
    sequence $ map mkParsingTest migrationParsingTestCases