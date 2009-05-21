module FilesystemTest
    ( tests
    )
where

import Test.HUnit
import Common
import System.FilePath ( (</>) )
import Data.Time.Clock ( UTCTime )

import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration

tests :: IO [Test]
tests = do
  parsingTests <- migrationParsingTests
  return $ parsingTests ++ serializationTests

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

fp :: FilePath -> FilePath
fp p = testFile $ "migration_parsing" </> p

migrationParsingTestCases :: [MigrationParsingTestCase]
migrationParsingTestCases = [ (fp "valid_full", Right valid_full)
                            , (fp "valid_with_comments"
                              , Right (valid_full { mId = "valid_with_comments" }))
                            , (fp "valid_no_depends"
                              , Right (valid_full { mId = "valid_no_depends", mDeps = [] }))
                            , (fp "valid_no_desc"
                              , Right (valid_full { mId = "valid_no_desc", mDesc = Nothing }))
                            , (fp "valid_no_revert"
                              , Right (valid_full { mId = "valid_no_revert", mRevert = Nothing }))
                            , (fp "invalid_missing_required_fields"
                              , Left $ "Missing required field(s) in migration " ++
                                         (show $ fp "invalid_missing_required_fields") ++
                                         ": [\"Created\",\"Depends\"]")
                            , (fp "invalid_deps_list"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_deps_list"))
                            , (fp "invalid_field_name"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_field_name"))
                            , (fp "invalid_syntax"
                              , Left $ "Could not parse migration file " ++
                                         (show $ fp "invalid_syntax"))
                            , (fp "invalid_timestamp"
                              , Left $ "Unrecognized field in migration " ++
                                         (show $ fp "invalid_timestamp"))
                            ]

mkParsingTest :: MigrationParsingTestCase -> IO Test
mkParsingTest (fname, expected) = do
  actual <- migrationFromFile fname
  return $ test $ expected ~=? actual

mkSerializationTest :: (Migration, String) -> Test
mkSerializationTest (m, expectedString) = test $ expectedString ~=? serializeMigration m

serializationTestCases :: [(Migration, String)]
serializationTestCases = [ (valid_full, "Description: A valid full migration.\n\
                                        \Created: " ++ tsStr ++ "\n\
                                        \Depends: another_migration\n\
                                        \Apply:\n\
                                        \  CREATE TABLE test (\n\
                                        \    a int\n\
                                        \  );\n\n\
                                        \Revert:\n\
                                        \  DROP TABLE test;\n")
                         , (valid_full { mDesc = Nothing }
                           , "Created: " ++ tsStr ++ "\n\
                             \Depends: another_migration\n\
                             \Apply:\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n\n\
                             \Revert:\n\
                             \  DROP TABLE test;\n")
                         , (valid_full { mDeps = ["one", "two"] }
                           , "Description: A valid full migration.\n\
                             \Created: " ++ tsStr ++ "\n\
                             \Depends: one two\n\
                             \Apply:\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n\n\
                             \Revert:\n\
                             \  DROP TABLE test;\n")
                         , (valid_full { mRevert = Nothing }
                           , "Description: A valid full migration.\n\
                             \Created: " ++ tsStr ++ "\n\
                             \Depends: another_migration\n\
                             \Apply:\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n")
                         ]

serializationTests :: [Test]
serializationTests = map mkSerializationTest serializationTestCases

migrationParsingTests :: IO [Test]
migrationParsingTests =
    sequence $ map mkParsingTest migrationParsingTestCases