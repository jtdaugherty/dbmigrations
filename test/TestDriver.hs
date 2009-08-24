module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified BackendTest
import qualified DependencyTest
import qualified MigrationsTest
import qualified FilesystemSerializeTest
import qualified FilesystemParseTest
import qualified FilesystemTest
import qualified CycleDetectionTest

import Control.Monad ( forM )

import Database.HDBC.Sqlite3 ( connectSqlite3 )
import Database.HDBC.PostgreSQL ( connectPostgreSQL )

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"
  pgConn <- connectPostgreSQL "dbname=cygnus"

  let backends = [ ("Sqlite", BackendTest.tests sqliteConn)
                 , ("PostgreSQL", BackendTest.tests pgConn)
                 ]

  backendTests <- forM backends $ \(name, testAct) -> do
                    theTests <- testAct
                    return $ name ~: test theTests

  ioTests <- sequence [ do fspTests <- FilesystemParseTest.tests
                           return $ "Filesystem Parsing" ~: test fspTests
                      , do fsTests <- FilesystemTest.tests
                           return $ "Filesystem general" ~: test fsTests
                      ]
  return $ concat [ backendTests
                  , ioTests
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
