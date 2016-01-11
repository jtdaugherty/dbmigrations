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
import qualified ConfigurationTest

import Control.Monad ( forM )
import Control.Exception ( finally, catch, try, SomeException(..) )
import Data.String (fromString)

import Database.HDBC ( IConnection(disconnect) )
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import qualified Database.HDBC.PostgreSQL as PostgreSQL
import qualified Database.MySQL.Simple as MySQL

loadTests :: IO [Test]
loadTests = do

  sqliteConn <- connectSqlite3 ":memory:"
  --pgConn <- setupPostgresDb
  mysqlConn <- setupMySQLDb

  let backends = [ ("Sqlite", (BackendTest.tests (BackendTest.HDBCConnection sqliteConn)) `finally`
                                (disconnect sqliteConn))
                 -- , ("PostgreSQL", (BackendTest.tests (BackendTest.HDBCConnection pgConn)) `finally`
                 --                    (disconnect pgConn >> teardownPostgresDb))
                 , ("MySQL", (BackendTest.tests (BackendTest.MySQLConnection mysqlConn)) `finally`
                               (MySQL.close mysqlConn >> teardownMySQLDb))
                 ]

  backendTests <- forM backends $ \(name, testAct) -> do
                    return $ (name ++ " backend tests") ~: test testAct

  ioTests <- sequence [ do fspTests <- FilesystemParseTest.tests
                           return $ "Filesystem Parsing" ~: test fspTests
                      , do fsTests <- FilesystemTest.tests
                           return $ "Filesystem general" ~: test fsTests
                      , do linTests <- LinearMigrationsTest.tests
                           return $ "Linear migrations" ~: test linTests
                      , do cfgTests <- ConfigurationTest.tests
                           return $ "Configuration tests" ~: test cfgTests
                      ]
  return $ concat [ backendTests
                  , ioTests
                  , DependencyTest.tests
                  , FilesystemSerializeTest.tests
                  , MigrationsTest.tests
                  , CycleDetectionTest.tests
                  , StoreTest.tests
                  ]

tempDatabase :: String
tempDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setupPostgresDb :: IO PostgreSQL.Connection
setupPostgresDb = do
  teardownPostgresDb `catch` ignoreException

  -- create database
  status <- system $ "createdb " ++ tempDatabase
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to create PostgreSQL database " ++ (show tempDatabase)

  -- return test db connection
  PostgreSQL.connectPostgreSQL $ "dbname=" ++ tempDatabase

teardownPostgresDb :: IO ()
teardownPostgresDb = do
  -- drop database
  status <- system $ "dropdb " ++ tempDatabase ++ " 2>/dev/null"
  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> error $ "Failed to drop PostgreSQL database " ++ (show tempDatabase)

setupMySQLDb :: IO MySQL.Connection
setupMySQLDb = do
  teardownMySQLDb `catch` ignoreException
  conn <- MySQL.connect MySQL.defaultConnectInfo
  MySQL.execute_ conn (fromString ("CREATE DATABASE " ++ tempDatabase))
  MySQL.execute_ conn (fromString ("USE " ++ tempDatabase))
  pure conn

teardownMySQLDb :: IO ()
teardownMySQLDb = do
  conn <- MySQL.connect MySQL.defaultConnectInfo
  e <- try (MySQL.execute_ conn (fromString ("DROP DATABASE " ++ tempDatabase)))
  case e of
    Left ex@SomeException{} -> error ("Failed to drop test MySQL database: " ++ show ex)
    Right _ -> return ()

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
