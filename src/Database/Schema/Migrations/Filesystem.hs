{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStore(..)
    , migrationFromFile
    , migrationFromPath
    )
where

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension
                       , takeFileName, takeBaseName )
import Control.Monad.Trans ( MonadIO, liftIO )

import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance

import Control.Monad ( filterM )

import Text.ParserCombinators.Parsec ( parse )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
import Database.Schema.Migrations.Filesystem.Parse
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

type FieldProcessor = String -> Migration -> Maybe Migration

data FilesystemStore = FSStore { storePath :: FilePath }

filenameExtension :: String
filenameExtension = ".txt"

instance (MonadIO m) => MigrationStore FilesystemStore m where
    fullMigrationName s name =
        return $ storePath s </> name ++ filenameExtension

    loadMigration s theId = do
      result <- liftIO $ migrationFromFile s theId
      return $ case result of
                 Left _ -> Nothing
                 Right m -> Just m

    getMigrations s = do
      contents <- liftIO $ getDirectoryContents $ storePath s
      let migrationFilenames = [ f | f <- contents, isMigrationFilename f ]
          fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
      existing <- liftIO $ filterM (\(_, full) -> doesFileExist full) fullPaths
      return [ dropExtension short | (short, _) <- existing ]

    saveMigration s m = do
      filename <- fullMigrationName s $ mId m
      liftIO $ writeFile filename $ serializeMigration m

isMigrationFilename :: FilePath -> Bool
isMigrationFilename path = takeExtension path == filenameExtension

-- |Given a store and migration name, read and parse the associated
-- migration and return the migration if successful.  Otherwise return
-- a parsing error message.
migrationFromFile :: FilesystemStore -> String -> IO (Either String Migration)
migrationFromFile store name =
    fullMigrationName store name >>= migrationFromPath

-- |Given a filesystem path, read and parse the file as a migration
-- return the 'Migration' if successful.  Otherwise return a parsing
-- error message.
migrationFromPath :: FilePath -> IO (Either String Migration)
migrationFromPath path = do
  contents <- readFile path
  let name = takeBaseName $ takeFileName path
  case parse migrationParser path contents of
    Left _ -> return $ Left $ "Could not parse migration file " ++ (show path)
    Right fields ->
        do
          let missing = missingFields fields
          case length missing of
            0 -> do
              newM <- newMigration ""
              case migrationFromFields newM fields of
                Nothing -> return $ Left $ "Unrecognized field in migration " ++ (show path)
                Just m -> return $ Right $ m { mId = name }
            _ -> return $ Left $ "Missing required field(s) in migration " ++ (show path) ++ ": " ++ (show missing)

missingFields :: FieldSet -> [FieldName]
missingFields fs =
    [ k | k <- requiredFields, not (k `elem` inputFieldNames) ]
    where
      inputFieldNames = [ n | (n, _) <- fs ]

-- |Given a migration and a list of parsed migration fields, update
-- the migration from the field values for recognized fields.
migrationFromFields :: Migration -> FieldSet -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

requiredFields :: [FieldName]
requiredFields = [ "Created"
                 , "Apply"
                 , "Depends"
                 ]

fieldProcessors :: [(FieldName, FieldProcessor)]
fieldProcessors = [ ("Created", setTimestamp )
                  , ("Description", setDescription )
                  , ("Apply", setApply )
                  , ("Revert", setRevert )
                  , ("Depends", setDepends )
                  ]

setTimestamp :: FieldProcessor
setTimestamp value m = do
  ts <- case readTimestamp value of
          [(t, _)] -> return t
          _ -> fail "expected one valid parse"
  return $ m { mTimestamp = ts }

readTimestamp :: String -> [(UTCTime, String)]
readTimestamp = reads

setDescription :: FieldProcessor
setDescription desc m = Just $ m { mDesc = Just desc }

setApply :: FieldProcessor
setApply apply m = Just $ m { mApply = apply }

setRevert :: FieldProcessor
setRevert revert m = Just $ m { mRevert = Just revert }

setDepends :: FieldProcessor
setDepends depString m = do
  case parse parseDepsList "-" depString of
    Left _ -> Nothing
    Right depIds -> Just $ m { mDeps = depIds }
