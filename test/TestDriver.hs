module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified SqliteTest
import qualified DependencyTest
import qualified MigrationsTest
import qualified FilesystemSerializeTest
import qualified FilesystemParseTest
import qualified FilesystemTest
import qualified CycleDetectionTest

import Database.HDBC.Sqlite3 ( connectSqlite3 )

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"

  ioTests <- sequence
             [ do sqliteTests <- SqliteTest.tests sqliteConn
                  return $ "Sqlite" ~: test sqliteTests
             , do fspTests <- FilesystemParseTest.tests
                  return $ "Filesystem Parsing" ~: test fspTests
             , do fsTests <- FilesystemTest.tests
                  return $ "Filesystem general" ~: test fsTests
             ]
  return $ concat [ ioTests
                  , DependencyTest.tests
                  , FilesystemSerializeTest.tests
                  , MigrationsTest.tests
                  , CycleDetectionTest.tests
                  ]

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
