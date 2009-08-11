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

loadTests :: IO [Test]
loadTests = do
  ioTests <- sequence
             [ do sqliteTests <- SqliteTest.tests
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
