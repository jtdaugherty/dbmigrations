module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified SqliteTest
import qualified DependencyTest
import qualified MigrationsTest
import qualified FilesystemSerializeTest
import qualified FilesystemParseTest

loadTests :: IO [Test]
loadTests = do
  ioTests <- sequence
             [ do sqliteTests <- SqliteTest.tests
                  return $ "Sqlite" ~: test sqliteTests
             , do mTests <- MigrationsTest.tests
                  return $ "Migrations" ~: test mTests
             , do fspTests <- FilesystemParseTest.tests
                  return $ "Filesystem Parsing" ~: test fspTests
             ]
  return $ ioTests ++ DependencyTest.tests ++ FilesystemSerializeTest.tests

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
