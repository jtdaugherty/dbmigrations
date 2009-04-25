module SqliteTest
    ( tests
    )
where

import Test.HUnit

import Database.Schema.Migrations.Backend.Sqlite
import Database.Schema.Migrations.Backend ( Backend(..) )
import Database.HDBC.Sqlite3 ( Connection, connectSqlite3 )
import Database.HDBC ( IConnection(..) )

import System.IO ( openTempFile, hClose )
import System.Directory ( removeFile )

import Control.Exception ( finally )

tests :: IO [Test]
tests = sequence [
         bootstrapTest
        ]

withTempDb :: (Connection -> IO Test) -> IO Test
withTempDb act = do
  (path, h) <- openTempFile "/tmp" "sqlite3test.tmp"
  hClose h
  conn <- connectSqlite3 path
  result <- act conn `finally` (disconnect conn >>
                                removeFile path)
  return result

bootstrapTest :: IO Test
bootstrapTest =
    withTempDb $ \conn -> do
      bs <- getBootstrapMigration conn
      applyMigration conn bs
      tables <- getTables conn
      return $ test $ ["installed_migrations"] ~=? tables