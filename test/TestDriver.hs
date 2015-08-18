module Main where
import Prelude hiding ( catch )
import Test.HUnit
import System.Exit
import System.Process ( system )
import System.IO ( stderr )

import qualified BackendTest
import qualified DependencyTest
import qualified MigrationsTest
import qualified FilesystemSerializeTest
import qualified FilesystemParseTest
import qualified FilesystemTest
import qualified CycleDetectionTest
import qualified StoreTest
import qualified LinearMigrationsTest

import Control.Monad ( forM )
import Control.Exception ( finally, catch, SomeException )

import Database.HDBC ( IConnection(disconnect) )
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import qualified Database.HDBC.PostgreSQL as PostgreSQL

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"
  pgConn <- setupPostgresDb

  let backends = [ ("Sqlite", (BackendTest.tests sqliteConn) `finally`
                                (disconnect sqliteConn))
                 , ("PostgreSQL", (BackendTest.tests pgConn) `finally`
                                    (disconnect pgConn >> teardownPostgresDb))
                 ]

  backendTests <- forM backends $ \(name, testAct) -> do
                    return $ (name ++ " backend tests") ~: test testAct

  ioTests <- sequence [ do fspTests <- FilesystemParseTest.tests
                           return $ "Filesystem Parsing" ~: test fspTests
                      , do fsTests <- FilesystemTest.tests
                           return $ "Filesystem general" ~: test fsTests
                      , do linTests <- LinearMigrationsTest.tests
                           return $ "Linear migrations" ~: test linTests
                      ]
  return $ concat [ backendTests
                  , ioTests
                  , DependencyTest.tests
                  , FilesystemSerializeTest.tests
                  , MigrationsTest.tests
                  , CycleDetectionTest.tests
                  , StoreTest.tests
                  ]

tempPgDatabase :: String
tempPgDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setupPostgresDb :: IO PostgreSQL.Connection
setupPostgresDb = do
  teardownPostgresDb `catch` ignoreException

  -- create database
  status <- system $ "createdb " ++ tempPgDatabase
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to create PostgreSQL database " ++ (show tempPgDatabase)

  -- return test db connection
  PostgreSQL.connectPostgreSQL $ "dbname=" ++ tempPgDatabase

teardownPostgresDb :: IO ()
teardownPostgresDb = do
  -- create database
  status <- system $ "dropdb " ++ tempPgDatabase ++ " 2>/dev/null"
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to drop PostgreSQL database " ++ (show tempPgDatabase)

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
