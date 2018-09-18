{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStoreSettings(..)
    , migrationFromFile
    , migrationFromPath
    , filesystemStore
    )
where

import Prelude

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension, takeBaseName )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BSC
import Data.String.Conversions ( cs, (<>) )

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
    , emptyMigration
    )
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

type FieldProcessor = Text -> Migration -> Maybe Migration

data FilesystemStoreSettings = FSStore { storePath :: FilePath }

data FilesystemStoreError = FilesystemStoreError String
                            deriving (Show, Typeable)

instance Exception FilesystemStoreError

throwFS :: String -> a
throwFS = throw . FilesystemStoreError

filenameExtension :: String
filenameExtension = ".yml"

filenameExtensionTxt :: String
filenameExtensionTxt = ".txt"

filesystemStore :: FilesystemStoreSettings -> MigrationStore
filesystemStore s =
    MigrationStore { fullMigrationName = fmap addNewMigrationExtension . fsFullMigrationName s

                   , loadMigration = \theId -> migrationFromFile s theId

                   , getMigrations = do
                       contents <- getDirectoryContents $ storePath s
                       let migrationFilenames = [ f | f <- contents, isMigrationFilename f ]
                           fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
                       existing <- filterM (\(_, full) -> doesFileExist full) fullPaths
                       return [ cs $ dropExtension short | (short, _) <- existing ]

                   , saveMigration = \m -> do
                       filename <- fsFullMigrationName s $ mId m
                       BSC.writeFile (cs $ addNewMigrationExtension filename) $ serializeMigration m
                   }

addNewMigrationExtension :: FilePath -> FilePath
addNewMigrationExtension path = path <> filenameExtension

addMigrationExtension :: FilePath -> String -> FilePath
addMigrationExtension path ext = path <> ext

-- |Build path to migrations without extension.
fsFullMigrationName :: FilesystemStoreSettings -> Text -> IO FilePath
fsFullMigrationName s name = return $ storePath s </> cs name

isMigrationFilename :: String -> Bool
isMigrationFilename path = (cs $ takeExtension path) `elem` [filenameExtension, filenameExtensionTxt]

-- |Given a store and migration name, read and parse the associated
-- migration and return the migration if successful.  Otherwise return
-- a parsing error message.
migrationFromFile :: FilesystemStoreSettings -> Text -> IO (Either String Migration)
migrationFromFile store name =
    fsFullMigrationName store (cs name) >>= migrationFromPath

-- |Given a filesystem path, read and parse the file as a migration
-- return the 'Migration' if successful.  Otherwise return a parsing
-- error message.
migrationFromPath :: FilePath -> IO (Either String Migration)
migrationFromPath path = do
  let name = cs $ takeBaseName path
  (Right <$> process name) `catch` (\(FilesystemStoreError s) -> return $ Left $ "Could not parse migration " ++ path ++ ":" ++ s)

  where
    readMigrationFile = do
      ymlExists <- doesFileExist (addNewMigrationExtension path)
      if ymlExists
        then parseYamlFile (addNewMigrationExtension path) `catch` (\(e::IOException) -> throwFS $ show e)
        else parseYamlFile (addMigrationExtension path filenameExtensionTxt) `catch` (\(e::IOException) -> throwFS $ show e)

    process name = do
      yaml <- readMigrationFile

      -- Convert yaml structure into basic key/value map
      let fields = getFields yaml
          missing = missingFields fields

      case length missing of
        0 -> do
          let newM = emptyMigration name
          case migrationFromFields newM fields of
            Nothing -> throwFS $ "Error in " ++ (show path) ++ ": unrecognized field found"
            Just m -> return m
        _ -> throwFS $ "Error in " ++ (show path) ++ ": missing required field(s): " ++ (show missing)

getFields :: YamlLight -> [(Text, Text)]
getFields (YMap mp) = map toPair $ Map.assocs mp
    where
      toPair (YStr k, YStr v) = (T.decodeUtf8 k, T.decodeUtf8 v)
      toPair (k, v) = throwFS $ "Error in YAML input; expected string key and string value, got " ++ (show (k, v))
getFields _ = throwFS "Error in YAML input; expected mapping"

missingFields :: [(Text, Text)] -> [Text]
missingFields fs =
    [ k | k <- requiredFields, not (k `elem` inputStrings) ]
    where
      inputStrings = map fst fs

-- |Given a migration and a list of parsed migration fields, update
-- the migration from the field values for recognized fields.
migrationFromFields :: Migration -> [(Text, Text)] -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

requiredFields :: [Text]
requiredFields = [ "Apply"
                 , "Depends"
                 ]

fieldProcessors :: [(Text, FieldProcessor)]
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
  return $ m { mTimestamp = Just ts }

readTimestamp :: Text -> [(UTCTime, String)]
readTimestamp = reads . cs

setDescription :: FieldProcessor
setDescription desc m = Just $ m { mDesc = Just desc }

setApply :: FieldProcessor
setApply apply m = Just $ m { mApply = apply }

setRevert :: FieldProcessor
setRevert revert m = Just $ m { mRevert = Just revert }

setDepends :: FieldProcessor
setDepends depString m = Just $ m { mDeps = T.words depString }
