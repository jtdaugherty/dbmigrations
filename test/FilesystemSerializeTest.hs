module FilesystemSerializeTest
    ( tests
    )
where

import Test.HUnit
import Data.Time.Clock ( UTCTime )

import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Migration

tests :: [Test]
tests = serializationTests

mkSerializationTest :: (Migration, String) -> Test
mkSerializationTest (m, expectedString) = test $ expectedString ~=? serializeMigration m

tsStr :: String
tsStr = "2009-04-15 10:02:06 UTC"

ts :: UTCTime
ts = read tsStr

valid_full :: Migration
valid_full = Migration {
               mTimestamp = Just ts
             , mId = "valid_full"
             , mDesc = Just "A valid full migration."
             , mDeps = ["another_migration"]
             , mApply = "  CREATE TABLE test (\n    a int\n  );\n"
             , mRevert = Just "DROP TABLE test;"
             }

serializationTestCases :: [(Migration, String)]
serializationTestCases = [ (valid_full, "Description: A valid full migration.\n\
                                        \Created: " ++ tsStr ++ "\n\
                                        \Depends: another_migration\n\
                                        \Apply: |\n\
                                        \  CREATE TABLE test (\n\
                                        \    a int\n\
                                        \  );\n\n\
                                        \Revert: |\n\
                                        \  DROP TABLE test;\n")
                         , (valid_full { mDesc = Nothing }
                           , "Created: " ++ tsStr ++ "\n\
                             \Depends: another_migration\n\
                             \Apply: |\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n\n\
                             \Revert: |\n\
                             \  DROP TABLE test;\n")
                         , (valid_full { mDeps = ["one", "two"] }
                           , "Description: A valid full migration.\n\
                             \Created: " ++ tsStr ++ "\n\
                             \Depends: one two\n\
                             \Apply: |\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n\n\
                             \Revert: |\n\
                             \  DROP TABLE test;\n")
                         , (valid_full { mRevert = Nothing }
                           , "Description: A valid full migration.\n\
                             \Created: " ++ tsStr ++ "\n\
                             \Depends: another_migration\n\
                             \Apply: |\n\
                             \  CREATE TABLE test (\n\
                             \    a int\n\
                             \  );\n")
                         ]

serializationTests :: [Test]
serializationTests = map mkSerializationTest serializationTestCases
