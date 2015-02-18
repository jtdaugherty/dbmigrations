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
             , mApply = "CREATE TABLE test ( a int );"
             , mRevert = Just "DROP TABLE test;"
             }

valid_full_comments :: Migration
valid_full_comments = Migration {
                        mTimestamp = ts
                      , mId = "valid_full"
                      , mDesc = Just "A valid full migration."
                      , mDeps = ["another_migration"]
                      , mApply = "\n-- Comment on a line\nCREATE TABLE test (\n  a int -- comment inline\n);\n"
                      , mRevert = Just "DROP TABLE test;"
                      }

valid_full_colon :: Migration
valid_full_colon = Migration {
                        mTimestamp = ts
                      , mId = "valid_full"
                      , mDesc = Just "A valid full migration."
                      , mDeps = ["another_migration"]
                      , mApply = "\n-- Comment on a line with a colon:\nCREATE TABLE test (\n  a int\n);\n"
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
                            , ("valid_with_comments2"
                              , Right (valid_full_comments { mId = "valid_with_comments2" }))
                            , ("valid_with_colon"
                              , Right (valid_full_colon { mId = "valid_with_colon" }))
                            , ("valid_with_multiline_deps"
                              , Right (valid_full { mId = "valid_with_multiline_deps"
                                                  , mDeps = ["one", "two", "three"] } ))
                            , ("valid_no_depends"
                              , Right (valid_full { mId = "valid_no_depends", mDeps = [] }))
                            , ("valid_no_desc"
                              , Right (valid_full { mId = "valid_no_desc", mDesc = Nothing }))
                            , ("valid_no_revert"
                              , Right (valid_full { mId = "valid_no_revert", mRevert = Nothing }))
                            , ("invalid_missing_required_fields"
                              , Left $ "Could not parse migration " ++
                                         (fp "invalid_missing_required_fields.txt") ++
                                         ":Error in " ++
                                         (show $ fp "invalid_missing_required_fields.txt") ++
                                         ": missing required field(s): " ++
                                         "[\"Created\",\"Depends\"]")
                            , ("invalid_field_name"
                              , Left $ "Could not parse migration " ++
                                         (fp "invalid_field_name.txt") ++
                                         ":Error in " ++
                                         (show $ fp "invalid_field_name.txt") ++
                                         ": unrecognized field found")
                            , ("invalid_syntax"
                              , Left $ "Could not parse migration " ++
                                         (fp "invalid_syntax.txt") ++
                                         ":user error (syntax error: line 7, " ++
                                         "column 0)")
                            , ("invalid_timestamp"
                              , Left $ "Could not parse migration " ++
                                         (fp "invalid_timestamp.txt") ++
                                         ":Error in " ++
                                         (show $ fp "invalid_timestamp.txt") ++
                                         ": unrecognized field found")
                            ]

mkParsingTest :: MigrationParsingTestCase -> IO Test
mkParsingTest (fname, expected) = do
  let store = FSStore { storePath = testStorePath }
  actual <- migrationFromFile store fname
  return $ test $ expected ~=? actual

migrationParsingTests :: IO [Test]
migrationParsingTests =
    sequence $ map mkParsingTest migrationParsingTestCases