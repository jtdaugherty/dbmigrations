module Database.Schema.Migrations.Filesystem
    ( newFilesystemStore

    , FilesystemStore
    , storePath
    , migrationMap

    , MigrationMap
    , migrationParser
    , migrationFromFile
    , FieldName
    , Field
    , FieldSet

    , serializeMigration
    )
where

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( takeDirectory, takeFileName, (</>) )

import qualified Data.Map as Map
import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance
import Data.Maybe ( isNothing, catMaybes )
import Data.List ( intercalate )

import Text.ParserCombinators.Parsec

import Control.Monad ( filterM, when, mapM_ )
import Control.Monad.Trans ( liftIO )
import Control.Monad.State ( StateT, get, put, execStateT )

import Database.Schema.Migrations.Migration
    ( Migration(..)
    , newMigration
    )

-- |Code for parsing and serializing Migrations to disk files, and an
-- instance of MigrationStore for filesystem-backed migrations.

type MigrationMap = Map.Map String Migration
type FieldName = String
type Field = (FieldName, String)
type FieldSet = [Field]
type FieldProcessor = String -> Migration -> Maybe Migration

data FilesystemStore = FSStore { storePath :: FilePath
                               , migrationMap :: MigrationMap }

-- |Create a new filesystem store by loading all migrations at the
-- specified filesystem path.
newFilesystemStore :: FilePath -> IO FilesystemStore
newFilesystemStore path = do
  migrations <- execStateT (loadMigrations path) Map.empty
  return $ FSStore { storePath = path
                   , migrationMap = migrations }

-- |Given a directory path, return a list of all files in the
-- directory, not including the special directories "." and "..".
filesInDirectory :: FilePath -> IO [FilePath]
filesInDirectory path = do
  contents <- getDirectoryContents path
  let withPath = map (path </>) nonSpecial
      nonSpecial = [ f | f <- contents, not (f `elem` [".", ".."]) ]
  liftIO $ filterM doesFileExist withPath

-- |Load migrations recursively from the specified path into the
-- MigrationMap state.
loadMigrations :: FilePath -> StateT MigrationMap IO ()
loadMigrations path = (liftIO $ filesInDirectory path) >>= mapM_ loadWithDeps

-- |Given a file path, return its corresponding migration ID.
migrationIdFromPath :: FilePath -> String
migrationIdFromPath = takeFileName

-- |Given a file path, load the migration at the specified path and,
-- if necessary, recursively load its dependencies into the
-- MigrationMap state.
loadWithDeps :: FilePath -> StateT MigrationMap IO ()
loadWithDeps path = do
  let parent = takeDirectory path
      mid = migrationIdFromPath path
  currentMap <- get
  when (isNothing $ Map.lookup mid currentMap) $
       do
         result <- liftIO $ migrationFromFile path
         case result of
           Left e -> fail ("Could not load migration from file " ++ path ++ ": " ++ e)
           Right m -> do
                      mapM_ (\p -> loadWithDeps $ parent </> p) $ mDeps m
                      newMap <- get
                      put $ Map.insert (mId m) m newMap

-- |Given a file path, read and parse the migration at the specified
-- path and, if successful, return the migration and its claimed
-- dependencies.
migrationFromFile :: FilePath -> IO (Either String Migration)
migrationFromFile path = do
  contents <- readFile path
  let migrationId = migrationIdFromPath path
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
                Just m -> return $ Right $ m { mId = migrationId }
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

-- |Parse a migration document and return a list of parsed fields and
-- a list of claimed dependencies.
migrationParser :: Parser [Field]
migrationParser = do
  result <- many (parseField <|> parseComment <|> parseEmptyLine)
  return $ catMaybes result

parseDepsList :: Parser [String]
parseDepsList =
    let parseMID = many1 (alphaNum <|> oneOf "-._")
    in do
      deps <- sepBy parseMID whitespace
      eol
      return deps

discard :: Parser a -> Parser ()
discard = (>> return ())

eol :: Parser ()
eol = (discard newline) <|> (discard eof)

whitespace :: Parser Char
whitespace = oneOf " \t"

requiredWhitespace :: Parser String
requiredWhitespace = many1 whitespace

parseFieldName :: Parser FieldName
parseFieldName = many1 (alphaNum <|> char '-')

parseComment :: Parser (Maybe Field)
parseComment = do
  discard $ do
    many whitespace
    char '#'
    manyTill anyChar eol
  return Nothing

parseEmptyLine :: Parser (Maybe Field)
parseEmptyLine = newline >> return Nothing

parseField :: Parser (Maybe Field)
parseField = do
  name <- parseFieldName
  char ':'
  many whitespace
  rest <- manyTill anyChar eol
  otherLines <- otherContentLines
  let value = rest ++ (concat otherLines)
  return $ Just (name, value)

otherContentLines :: Parser [String]
otherContentLines =
    many $ try $ (discard newline >> return "") <|> do
      ws <- requiredWhitespace
      rest <- manyTill anyChar eol
      -- Retain leading whitespace and trailing newline
      return $ ws ++ rest ++ "\n"

type FieldSerializer = Migration -> Maybe String

fieldSerializers :: [FieldSerializer]
fieldSerializers = [ serializeDesc
                   , serializeTimestamp
                   , serializeDepends
                   , serializeApply
                   , serializeRevert
                   ]

serializeDesc :: FieldSerializer
serializeDesc m =
    case mDesc m of
      Nothing -> Nothing
      Just desc -> Just $ "Description: " ++ desc

serializeTimestamp :: FieldSerializer
serializeTimestamp m = Just $ "Created: " ++ (show $ mTimestamp m)

serializeDepends :: FieldSerializer
serializeDepends m = Just $ "Depends: " ++ (intercalate " " $ mDeps m)

serializeRevert :: FieldSerializer
serializeRevert m =
    case mRevert m of
      Nothing -> Nothing
      Just revert -> Just $ "Revert:\n" ++
                     (serializeMultiline revert)

serializeApply :: FieldSerializer
serializeApply m = Just $ "Apply:\n" ++ (serializeMultiline $ mApply m)

commonPrefix :: String -> String -> String
commonPrefix a b = map fst $ takeWhile (uncurry (==)) (zip a b)

commonPrefixLines :: [String] -> String
commonPrefixLines [] = ""
commonPrefixLines theLines = foldl1 commonPrefix theLines

serializeMultiline :: String -> String
serializeMultiline s =
    let sLines = lines s
        prefix = case commonPrefixLines sLines of
                   -- If the lines already have a common prefix that
                   -- begins with whitespace, no new prefix is
                   -- necessary.
                   (' ':_) -> ""
                   -- Otherwise, use a new prefix of two spaces.
                   _ -> "  "

    in unlines $ map (prefix ++) sLines

serializeMigration :: Migration -> String
serializeMigration m = intercalate "\n" fields
    where
      fields = catMaybes [ f m | f <- fieldSerializers ]
