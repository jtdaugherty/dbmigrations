{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStore(..)
    , migrationFromFile
    , migrationFromPath
    )
where

import Prelude hiding ( catch )

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension
                       , takeFileName, takeBaseName )
import Data.ByteString.Char8 ( unpack )

import Data.Typeable ( Typeable )
import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance
import qualified Data.Map as Map

import Control.Applicative ( (<$>) )
import Control.Monad ( filterM )
import Control.Exception ( IOException, Exception(..), throw, catch )

import Data.Yaml.YamlLight

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

type FieldProcessor = String -> Migration -> Maybe Migration

data FilesystemStore = FSStore { storePath :: FilePath }

data FilesystemStoreError = FilesystemStoreError String
                            deriving (Show, Typeable)

instance Exception FilesystemStoreError

throwFS :: String -> a
throwFS = throw . FilesystemStoreError

filenameExtension :: String
filenameExtension = ".txt"

instance MigrationStore FilesystemStore where
    fullMigrationName s name =
        return $ storePath s </> name ++ filenameExtension

    loadMigration s theId = migrationFromFile s theId

    getMigrations s = do
      contents <- getDirectoryContents $ storePath s
      let migrationFilenames = [ f | f <- contents, isMigrationFilename f ]
          fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
      existing <- filterM (\(_, full) -> doesFileExist full) fullPaths
      return [ dropExtension short | (short, _) <- existing ]

    saveMigration s m = do
      filename <- fullMigrationName s $ mId m
      writeFile filename $ serializeMigration m

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
  let name = takeBaseName $ takeFileName path
  (Right <$> process name) `catch` (\(FilesystemStoreError s) -> return $ Left $ "Could not parse migration " ++ path ++ ":" ++ s)

  where
    process name = do
      yaml <- parseYamlFile path `catch` (\(e::IOException) -> throwFS $ show e)

      -- Convert yaml structure into basic key/value map
      let fields = getFields yaml
          missing = missingFields fields

      case length missing of
        0 -> do
          newM <- newMigration ""
          case migrationFromFields newM fields of
            Nothing -> throwFS $ "Error in " ++ (show path) ++ ": unrecognized field found"
            Just m -> return $ m { mId = name }
        _ -> throwFS $ "Error in " ++ (show path) ++ ": missing required field(s): " ++ (show missing)

getFields :: YamlLight -> [(String, String)]
getFields (YMap mp) = map toPair $ Map.assocs mp
    where
      toPair (YStr k, YStr v) = (unpack k, unpack v)
      toPair (k, v) = throwFS $ "Error in YAML input; expected string key and string value, got " ++ (show (k, v))
getFields _ = throwFS "Error in YAML input; expected mapping"

missingFields :: [(String, String)] -> [String]
missingFields fs =
    [ k | k <- requiredFields, not (k `elem` inputStrings) ]
    where
      inputStrings = map fst fs

-- |Given a migration and a list of parsed migration fields, update
-- the migration from the field values for recognized fields.
migrationFromFields :: Migration -> [(String, String)] -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

requiredFields :: [String]
requiredFields = [ "Created"
                 , "Apply"
                 , "Depends"
                 ]

fieldProcessors :: [(String, FieldProcessor)]
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
setDepends depString m = Just $ m { mDeps = words depString }
