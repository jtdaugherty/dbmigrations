{-# LANGUAGE FlexibleContexts #-}

module Moo.CommandUtils
       ( apply
       , confirmCreation
       , interactiveAskDeps
       , lookupMigration
       , revert
       , withConnection
       ) where

import Control.Exception ( bracket )
import Control.Monad ( when, forM_, unless )
import Control.Monad.Reader ( asks )
import Control.Monad.Trans ( liftIO )
import Data.List ( intercalate, sortBy )
import Data.Maybe ( fromJust, isNothing, isJust )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( stdout, hFlush, hGetBuffering
                 , hSetBuffering, stdin, BufferMode(..) )
import Database.HDBC ( IConnection, disconnect )


import Database.Schema.Migrations ( migrationsToApply, migrationsToRevert )
import Database.Schema.Migrations.Backend ( Backend
                                          , applyMigration
                                          , revertMigration
                                          )
import Database.Schema.Migrations.Migration ( Migration(..) )
import Database.Schema.Migrations.Store ( StoreData
                                        , storeLookup
                                        , storeMigrations
                                        )
import Moo.Core


apply :: (IConnection b, Backend b IO)
         => Migration -> StoreData -> b -> Bool -> IO [Migration]
apply m storeData backend complain = do
  -- Get the list of migrations to apply
  toApply <- migrationsToApply storeData backend m

  -- Apply them
  if null toApply then
      nothingToDo >> return [] else
      mapM_ (applyIt backend) toApply >> return toApply

    where
      nothingToDo =
        when complain $
             putStrLn $ "Nothing to do; " ++
                          mId m ++
                          " already installed."

      applyIt conn it = do
        putStr $ "Applying: " ++ mId it ++ "... "
        applyMigration conn it
        putStrLn "done."


revert :: (IConnection b, Backend b IO)
          => Migration -> StoreData -> b -> IO [Migration]
revert m storeData backend = do
  -- Get the list of migrations to revert
  toRevert <- liftIO $ migrationsToRevert storeData backend m

  -- Revert them
  if null toRevert then
      nothingToDo >> return [] else
      mapM_ (revertIt backend) toRevert >> return toRevert

    where
      nothingToDo =
        putStrLn $ "Nothing to do; " ++
                 mId m ++
                 " not installed."

      revertIt conn it = do
        putStr $ "Reverting: " ++ mId it ++ "... "
        revertMigration conn it
        putStrLn "done."


lookupMigration :: StoreData -> String -> IO Migration
lookupMigration storeData name = do
  let theMigration = storeLookup storeData name
  case theMigration of
    Nothing -> do
      putStrLn $ "No such migration: " ++ name
      exitWith (ExitFailure 1)
    Just m' -> return m'


-- Given a database type string and a database connection string,
-- return a database connection or raise an error if the database
-- connection cannot be established, or if the database type is not
-- supported.
makeConnection :: String -> DbConnDescriptor -> IO AnyIConnection
makeConnection dbType (DbConnDescriptor connStr) =
    case lookup dbType databaseTypes of
      Nothing -> error $ "Unsupported database type " ++ show dbType ++
                 " (supported types: " ++
                 intercalate "," (map fst databaseTypes) ++ ")"
      Just mkConnection -> mkConnection connStr


-- Given an action that needs a database connection, connect to the
-- database using the application configuration and invoke the action
-- with the connection.  Return its result.
withConnection :: (AnyIConnection -> IO a) -> AppT a
withConnection act = do
  mDbPath <- asks _appDatabaseConnStr
  when (isNothing mDbPath) $ error $ "Error: Database connection string not \
                                     \specified, please set " ++ envDatabaseName
  mDbType <- asks _appDatabaseType
  when (isNothing mDbType) $
       error $ "Error: Database type not specified, " ++
                 "please set " ++ envDatabaseType ++
                 " (supported types: " ++
                 intercalate "," (map fst databaseTypes) ++ ")"
  liftIO $ bracket (makeConnection (fromJust mDbType) (fromJust mDbPath))
             (\(AnyIConnection conn) -> disconnect conn) act


-- Given a migration name and selected dependencies, get the user's
-- confirmation that a migration should be created.
confirmCreation :: String -> [String] -> IO Bool
confirmCreation migrationId deps = do
  putStrLn ""
  putStrLn $ "Confirm: create migration '" ++ migrationId ++ "'"
  if null deps then putStrLn "  (No dependencies)"
     else putStrLn "with dependencies:"
  forM_ deps $ \d -> putStrLn $ "  " ++ d
  prompt "Are you sure?" [ ('y', (True, Nothing))
                         , ('n', (False, Nothing))
                         ]


-- Prompt the user for a choice, given a prompt and a list of possible
-- choices.  Let the user get help for the available choices, and loop
-- until the user makes a valid choice.
prompt :: (Eq a) => String -> PromptChoices a -> IO a
prompt _ [] = error "prompt requires a list of choices"
prompt message choiceMap = do
  putStr $ message ++ " (" ++ choiceStr ++ helpChar ++ "): "
  hFlush stdout
  c <- unbufferedGetChar
  case lookup c choiceMap of
    Nothing -> do
      when (c /= '\n') $ putStrLn ""
      when (c == 'h') $ putStr $ mkPromptHelp choiceMapWithHelp
      retry
    Just (val, _) -> putStrLn "" >> return val
    where
      retry = prompt message choiceMap
      choiceStr = intercalate "" $ map (return . fst) choiceMap
      helpChar = if hasHelp choiceMap then "h" else ""
      choiceMapWithHelp = choiceMap ++ [('h', (undefined, Just "this help"))]


-- Given a PromptChoices, build a multi-line help string for those
-- choices using the description information in the choice list.
mkPromptHelp :: PromptChoices a -> String
mkPromptHelp choices =
    intercalate "" [ [c] ++ ": " ++ fromJust msg ++ "\n" |
                     (c, (_, msg)) <- choices, isJust msg ]


-- Does the specified prompt choice list have any help messages in it?
hasHelp :: PromptChoices a -> Bool
hasHelp = (> 0) . length . filter hasMsg
    where hasMsg (_, (_, m)) = isJust m


-- A general type for a set of choices that the user can make at a
-- prompt.
type PromptChoices a = [(Char, (a, Maybe String))]


-- Get an input character in non-buffered mode, then restore the
-- original buffering setting.
unbufferedGetChar :: IO Char
unbufferedGetChar = do
  bufferingMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin bufferingMode
  return c


-- The types for choices the user can make when being prompted for
-- dependencies.
data AskDepsChoice = Yes | No | View | Done | Quit
                     deriving (Eq)


-- Interactively ask the user about which dependencies should be used
-- when creating a new migration.
interactiveAskDeps :: StoreData -> IO [String]
interactiveAskDeps storeData = do
  -- For each migration in the store, starting with the most recently
  -- added, ask the user if it should be added to a dependency list
  let sorted = sortBy compareTimestamps $ storeMigrations storeData
  interactiveAskDeps' storeData (map mId sorted)
      where
        compareTimestamps m1 m2 = compare (mTimestamp m2) (mTimestamp m1)


-- Recursive function to prompt the user for dependencies and let the
-- user view information about potential dependencies.  Returns a list
-- of migration names which were selected.
interactiveAskDeps' :: StoreData -> [String] -> IO [String]
interactiveAskDeps' _ [] = return []
interactiveAskDeps' storeData (name:rest) = do
  result <- prompt ("Depend on '" ++ name ++ "'?") askDepsChoices
  if result == Done then return [] else
        case result of
          Yes -> do
            next <- interactiveAskDeps' storeData rest
            return $ name:next
          No -> interactiveAskDeps' storeData rest
          View -> do
            -- load migration
            let Just m = storeLookup storeData name
            -- print out description, timestamp, deps
            when (isJust $ mDesc m)
                     (putStrLn $ "  Description: " ++
                                    fromJust  (mDesc m))
            putStrLn $ "      Created: " ++ show (mTimestamp m)
            unless (null $ mDeps m)
                     (putStrLn $ "  Deps: " ++
                                   intercalate "\n        "  (mDeps m))
            -- ask again
            interactiveAskDeps' storeData (name:rest)
          Quit -> do
            putStrLn "cancelled."
            exitWith (ExitFailure 1)
          Done -> return []


-- The choices the user can make when being prompted for dependencies.
askDepsChoices :: PromptChoices AskDepsChoice
askDepsChoices = [ ('y', (Yes, Just "yes, depend on this migration"))
                 , ('n', (No, Just "no, do not depend on this migration"))
                 , ('v', (View, Just "view migration details"))
                 , ('d', (Done, Just "done, do not ask me about more dependencies"))
                 , ('q', (Quit, Just "cancel this operation and quit"))
                 ]
