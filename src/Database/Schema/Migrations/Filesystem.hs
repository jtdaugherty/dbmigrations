{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as BSC
import Data.String.Conversions ( cs, (<>) )

import Data.Typeable ( Typeable )
import Data.Time.Clock ( UTCTime )
import Data.Time ( defaultTimeLocale, formatTime, parseTimeM )
import qualified Data.Map as Map

import Control.Applicative ( (<$>), (<|>) )
import Control.Monad ( filterM )
import Control.Exception ( Exception(..), throw, catch )

import Data.Aeson
import Data.HashMap.Strict as M (toList)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

import Database.Schema.Migrations.Migration (Migration(..))
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

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
        then Yaml.decodeFileThrow (addNewMigrationExtension path) `catch` (\(e::Yaml.ParseException) -> throwFS $ show e)
        else Yaml.decodeFileThrow (addMigrationExtension path filenameExtensionTxt) `catch` (\(e::Yaml.ParseException) -> throwFS $ show e)

    process name = migrationYamlToMigration name <$> readMigrationFile

-- | TODO: re-use this for the generation side too
data MigrationYaml = MigrationYaml
    { myCreated :: Maybe UTCTimeYaml
    , myDescription :: Maybe Text
    , myApply :: Text
    , myRevert :: Maybe Text
    , myDepends :: Text
    -- ^ White-space separated names
    }
    deriving Generic

instance FromJSON MigrationYaml where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON MigrationYaml where
    toJSON = genericToJSON jsonOptions
    toEncoding = genericToEncoding jsonOptions

jsonOptions :: Options
jsonOptions = defaultOptions
    { fieldLabelModifier = drop 2 -- remove "my" prefix
    , omitNothingFields = True
    , rejectUnknownFields = True
    }

migrationYamlToMigration :: Text -> MigrationYaml -> Migration
migrationYamlToMigration theId my = Migration
    { mTimestamp = unUTCTimeYaml <$> myCreated my
    , mId = theId
    , mDesc = myDescription my
    , mApply = myApply my
    , mRevert = myRevert my
    , mDeps = T.words $ myDepends my
    }

newtype UTCTimeYaml = UTCTimeYaml
    { unUTCTimeYaml :: UTCTime
    }

instance FromJSON UTCTimeYaml where
    parseJSON = withText "UTCTime"
        $ maybe (fail "Unable to parse UTCTime") (pure . UTCTimeYaml)
        . parseTimeM True defaultTimeLocale utcTimeYamlFormat
        . cs

instance ToJSON UTCTimeYaml where
    toJSON = toJSON . formatTime defaultTimeLocale utcTimeYamlFormat . unUTCTimeYaml
    toEncoding = toEncoding . formatTime defaultTimeLocale utcTimeYamlFormat . unUTCTimeYaml

-- Keeps things as the old Show/Read-based format, e.g "2009-04-15 10:02:06 UTC"
utcTimeYamlFormat :: String
utcTimeYamlFormat = "%F %T UTC"
