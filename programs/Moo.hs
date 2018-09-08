module Main
    ( main
    )
where

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Prelude  hiding (lookup)
import System.Environment (getArgs)
import System.Exit

import Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import Moo.Core
import Moo.Main

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  loadedConf <- loadConfiguration $ _configFilePath opts
  case loadedConf of
    Left e -> putStrLn e >> exitFailure
    Right conf -> do
      let connectionString = _connectionString conf
      connection <- connectPostgreSQL connectionString
      let backend = hdbcBackend connection
          parameters = makeParameters conf backend
      mainWithParameters args parameters
