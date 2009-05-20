module FilesystemTest
    ( tests
    )
where

import Test.HUnit
import Common
import System.FilePath ( (</>) )

import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration

tests :: IO [Test]
tests = migrationParsingTests

-- filename, result
type MigrationParsingTestCase = (FilePath, Maybe Migration)

valid_full :: Migration
valid_full = Migration {
               mTimestamp = read "2009-04-15 10:02:06 UTC"
             , mId = "valid_full"
             , mDesc = Just "A valid full migration."
             , mDeps = ["another_migration"]
             , mApply = "  CREATE TABLE test (\n    a int\n  );\n"
             , mRevert = Just "DROP TABLE test;"
             }

fp :: FilePath -> FilePath
fp p = testFile $ "migration_parsing" </> p

migrationParsingTestCases :: [MigrationParsingTestCase]
migrationParsingTestCases = [ (fp "valid_full", Just valid_full)
                            ]

mkParsingTest :: MigrationParsingTestCase -> IO Test
mkParsingTest (fname, expected) = do
  actual <- migrationFromFile fname
  return $ test $ expected ~=? actual

migrationParsingTests :: IO [Test]
migrationParsingTests =
    sequence $ map mkParsingTest migrationParsingTestCases