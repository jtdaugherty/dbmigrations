{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}
module Main
    ( main )
where
import System.Environment ( getArgs, getEnvironment, getProgName )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )
import System.IO ( stdout, hFlush, hGetBuffering
                 , hSetBuffering, stdin, BufferMode(..)
                 )
import Control.Exception ( bracket )
import Data.Maybe ( listToMaybe, catMaybes, isJust
                  , fromJust, isNothing
                  )
import Data.List ( intercalate, sortBy, isPrefixOf )
import Control.Monad.Reader ( ReaderT, asks, runReaderT )
import Control.Monad ( when, forM_ )
import Control.Monad.Trans ( liftIO )
import Control.Applicative ( (<$>) )
import Database.HDBC.PostgreSQL ( connectPostgreSQL )
import Database.HDBC.Sqlite3 ( connectSqlite3 )
import Database.HDBC ( IConnection(commit, rollback, disconnect)
                     , catchSql, seErrorMsg, SqlError
                     )
import Database.Schema.Migrations
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration ( Migration(..) )
import Database.Schema.Migrations.Backend ( Backend, applyMigration
                                          , revertMigration
                                          )
import Database.Schema.Migrations.Store ( loadMigrations
                                        , fullMigrationName
                                        , StoreData
                                        , storeMigrations
                                        , storeLookup
                                        )
import Database.Schema.Migrations.Backend.HDBC ()

-- A command has a name, a number of required arguments' labels, a
-- number of optional arguments' labels, and an action to invoke.
data Command = Command { cName :: String
                       , cRequired :: [String]
                       , cOptional :: [String]
                       , cAllowedOptions :: [CommandOption]
                       , cDescription :: String
                       , cHandler :: CommandHandler
                       }

newtype DbConnDescriptor = DbConnDescriptor String

-- Application state which can be accessed by any command handler.
data AppState = AppState { appOptions :: [CommandOption]
                         , appCommand :: Command
                         , appRequiredArgs :: [String]
                         , appOptionalArgs :: [String]
                         , appStore :: FilesystemStore
                         , appDatabaseConnStr :: Maybe DbConnDescriptor
                         , appDatabaseType :: Maybe String
                         , appStoreData :: StoreData
                         }

-- The monad in which the application runs.
type AppT a = ReaderT AppState IO a

-- The type of actions that are invoked to handle specific commands
type CommandHandler = StoreData -> AppT ()

-- Options which can be used to alter command behavior
data CommandOption = Test
                   | NoAsk
                   deriving (Eq)

-- Type wrapper for IConnection instances so the makeConnection
-- function can return any type of connection.
data AnyIConnection = forall c. (IConnection c) => AnyIConnection c

-- The types for choices the user can make when being prompted for
-- dependencies.
data AskDepsChoice = Yes | No | View | Done | Quit
                     deriving (Eq)

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

-- Given a PromptChoices, build a multi-line help string for those
-- choices using the description information in the choice list.
mkPromptHelp :: PromptChoices a -> String
mkPromptHelp choices =
    intercalate "" [ [c] ++ ": " ++ (fromJust msg) ++ "\n" |
                     (c, (_, msg)) <- choices, isJust msg ]

-- Does the specified prompt choice list have any help messages in it?
hasHelp :: PromptChoices a -> Bool
hasHelp = (> 0) . length . (filter hasMsg)
    where hasMsg (_, (_, m)) = isJust m

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

-- Different command-line option strings and their constructors.
optionMap :: [(String, CommandOption)]
optionMap = [ ("--test", Test)
            , ("--no-ask", NoAsk)]

-- Usage information for each CommandOption.
optionUsage :: CommandOption -> String
optionUsage Test = "Perform the action in a transaction and issue a \
                   \rollback when finished"
optionUsage NoAsk = "Do not interactively ask any questions, just do it"

-- Is the specified option turned on in the application state?
hasOption :: CommandOption -> AppT Bool
hasOption o = asks ((o `elem`) . appOptions)

-- Is the specified option string mapped to an option constructor?
isSupportedCommandOption :: String -> Bool
isSupportedCommandOption s = isJust $ lookup s optionMap

-- Does the specified string appear to be a command-line option?
isCommandOption :: String -> Bool
isCommandOption s = "--" `isPrefixOf` s

-- Given a list of strings, convert it into a list of well-typed
-- command options and remaining arguments, or return an error if any
-- options are unsupported.
convertOptions :: [String] -> Either String ([CommandOption], [String])
convertOptions args = if null unsupportedOptions
                      then Right (supportedOptions, rest)
                      else Left unsupported
    where
      allOptions = filter isCommandOption args
      supportedOptions = catMaybes $ map (\s -> lookup s optionMap) args
      unsupportedOptions = [ s | s <- allOptions
                           , not $ isSupportedCommandOption s ]
      rest = [arg | arg <- args, not $ isCommandOption arg]
      unsupported = "Unsupported option(s): "
                    ++ intercalate ", " unsupportedOptions

-- The available commands; used to dispatch from the command line and
-- used to generate usage output.
commands :: [Command]
commands = [ Command "new" ["migration_name"] [] [NoAsk]
                         "Create a new empty migration"
                         newCommand
           , Command "apply" ["migration_name"] [] []
                         "Apply the specified migration and its dependencies"
                         applyCommand
           , Command "revert" ["migration_name"] [] []
                         "Revert the specified migration and those that depend\
                          \ on it"
                          revertCommand
           , Command "test" ["migration_name"] [] []
                         "Test the specified migration by applying and \
                         \reverting it in a transaction, then roll back"
                         testCommand
           , Command "upgrade" [] [] [Test]
                         "Install all migrations that have not yet been \
                         \installed"
                         upgradeCommand
           , Command "upgrade-list" [] [] []
                         "Show the list of migrations to be installed during \
                         \an upgrade"
                         upgradeListCommand
           ]

-- The values of DBM_DATABASE_TYPE and their corresponding connection
-- factory functions.
databaseTypes :: [(String, String -> IO AnyIConnection)]
databaseTypes = [ ("postgresql", fmap AnyIConnection . connectPostgreSQL)
                , ("sqlite3", fmap AnyIConnection . connectSqlite3)
                ]

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
  mDbPath <- asks appDatabaseConnStr
  when (isNothing mDbPath) $ error $ "Error: Database connection string not \
                                     \specified, please set " ++ envDatabaseName

  mDbType <- asks appDatabaseType
  when (isNothing mDbType) $
       error $ "Error: Database type not specified, " ++
                 "please set " ++ envDatabaseType ++
                 " (supported types: " ++
                 intercalate "," (map fst databaseTypes) ++ ")"

  liftIO $ bracket (makeConnection (fromJust mDbType) (fromJust mDbPath))
             (\(AnyIConnection conn) -> disconnect conn) act

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

-- The choices the user can make when being prompted for dependencies.
askDepsChoices :: PromptChoices AskDepsChoice
askDepsChoices = [ ('y', (Yes, Just "yes, depend on this migration"))
                 , ('n', (No, Just "no, do not depend on this migration"))
                 , ('v', (View, Just "view migration details"))
                 , ('d', (Done, Just "done, do not ask me about more dependencies"))
                 , ('q', (Quit, Just "cancel this operation and quit"))
                 ]

-- Recursive function to prompt the user for dependencies and let the
-- user view information about potential dependencies.  Returns a list
-- of migration names which were selected.
interactiveAskDeps' :: StoreData -> [String] -> IO [String]
interactiveAskDeps' _ [] = return []
interactiveAskDeps' storeData (name:rest) = do
  result <- prompt ("Depend on '" ++ name ++ "'?") askDepsChoices
  if (result == Done) then return [] else
      do
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
                                   (fromJust $ mDesc m))
            putStrLn $ "      Created: " ++ (show $ mTimestamp m)
            when (not $ null $ mDeps m)
                     (putStrLn $ "  Deps: " ++
                                   (intercalate "\n        " $ mDeps m))
            -- ask again
            interactiveAskDeps' storeData (name:rest)
          Quit -> do
            putStrLn "cancelled."
            exitWith (ExitFailure 1)
          Done -> return []

-- Given a migration name and selected dependencies, get the user's
-- confirmation that a migration should be created.
confirmCreation :: String -> [String] -> IO Bool
confirmCreation migrationId deps = do
  putStrLn ""
  putStrLn $ "Confirm: create migration '" ++ migrationId ++ "'"
  if (null deps) then putStrLn "  (No dependencies)"
     else putStrLn "with dependencies:"
  forM_ deps $ \d -> putStrLn $ "  " ++ d
  prompt "Are you sure?" [ ('y', (True, Nothing))
                         , ('n', (False, Nothing))
                         ]

newCommand :: CommandHandler
newCommand storeData = do
  required <- asks appRequiredArgs
  store <- asks appStore
  let [migrationId] = required
  noAsk <- hasOption NoAsk

  liftIO $ do
    fullPath <- fullMigrationName store migrationId

    when (isJust $ storeLookup storeData migrationId) $
         do
           putStrLn $ "Migration " ++ (show fullPath) ++ " already exists"
           exitWith (ExitFailure 1)

    -- Default behavior: ask for dependencies
    deps <- if noAsk then (return []) else
            do
              putStrLn $ "Selecting dependencies for new \
                         \migration: " ++ migrationId
              interactiveAskDeps storeData

    result <- if noAsk then (return True) else
              (confirmCreation migrationId deps)

    case result of
      True -> do
               status <- createNewMigration store migrationId deps
               case status of
                 Left e -> putStrLn e >> (exitWith (ExitFailure 1))
                 Right _ -> putStrLn $ "Migration created successfully: " ++
                            show fullPath
      False -> do
               putStrLn "Migration creation cancelled."

upgradeCommand :: CommandHandler
upgradeCommand storeData = do
  isTesting <- hasOption Test
  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn storeData
        when (null migrationNames) $ do
                           putStrLn "Database is up to date."
                           exitSuccess
        forM_ migrationNames $ \migrationName -> do
            m <- lookupMigration storeData migrationName
            apply m storeData conn
        case isTesting of
          True -> do
                 rollback conn
                 putStrLn "Upgrade test successful."
          False -> do
                 commit conn
                 putStrLn "Database successfully upgraded."

upgradeListCommand :: CommandHandler
upgradeListCommand storeData = do
  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        migrationNames <- missingMigrations conn storeData
        when (null migrationNames) $ do
                               putStrLn "Database is up to date."
                               exitSuccess
        putStrLn "Migrations to install:"
        forM_ migrationNames (putStrLn . ("  " ++))

reportSqlError :: SqlError -> IO a
reportSqlError e = do
  putStrLn $ "\n" ++ "A database error occurred: " ++ seErrorMsg e
  exitWith (ExitFailure 1)

apply :: (IConnection b, Backend b IO)
         => Migration -> StoreData -> b -> IO [Migration]
apply m storeData backend = do
  -- Get the list of migrations to apply
  toApply <- migrationsToApply storeData backend m

  -- Apply them
  if (null toApply) then
      (nothingToDo >> return []) else
      mapM_ (applyIt backend) toApply >> return toApply

    where
      nothingToDo = do
        putStrLn $ "Nothing to do; " ++
                     (mId m) ++
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
  if (null toRevert) then
      (nothingToDo >> return []) else
      mapM_ (revertIt backend) toRevert >> return toRevert

    where
      nothingToDo = do
        putStrLn $ "Nothing to do; " ++
                   (mId m) ++
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

applyCommand :: CommandHandler
applyCommand storeData = do
  required <- asks appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration storeData migrationId
        apply m storeData conn
        commit conn
        putStrLn "Successfully applied migrations."

revertCommand :: CommandHandler
revertCommand storeData = do
  required <- asks appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
      ensureBootstrappedBackend conn >> commit conn
      m <- lookupMigration storeData migrationId
      revert m storeData conn
      commit conn
      putStrLn "Successfully reverted migrations."

testCommand :: CommandHandler
testCommand storeData = do
  required <- asks appRequiredArgs
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration storeData migrationId
        migrationNames <- missingMigrations conn storeData
        -- If the migration is already installed, remove it as part of
        -- the test
        when (not $ migrationId `elem` migrationNames) $
             do revert m storeData conn
                return ()
        applied <- apply m storeData conn
        forM_ (reverse applied) $ \migration -> do
                             revert migration storeData conn
        rollback conn
        putStrLn "Successfully tested migrations."

usageString :: Command -> String
usageString command =
    intercalate " " ((cName command):requiredArgs ++
                                    optionalArgs ++ options)
    where
      requiredArgs = map (\s -> "<" ++ s ++ ">") $ cRequired command
      optionalArgs = map (\s -> "[" ++ s ++ "]") $ cOptional command
      options = map (\s -> "[" ++ s ++ "]") $ optionStrings
      optionStrings = map (\o -> fromJust $ lookup o flippedOptions) $
                      cAllowedOptions command
      flippedOptions = map (\(a,b) -> (b,a)) optionMap

usage :: IO a
usage = do
  progName <- getProgName

  putStrLn $ "Usage: " ++ progName ++ " <command> [args]"
  putStrLn "Environment:"
  putStrLn $ "  " ++ envDatabaseName ++ ": database connection string"
  putStrLn $ "  " ++ envDatabaseType ++ ": database type, one of " ++
               (intercalate "," $ map fst databaseTypes)
  putStrLn $ "  " ++ envStoreName ++ ": path to migration store"
  putStrLn "Commands:"
  forM_ commands $ \command -> do
          putStrLn $ "  " ++ usageString command
          putStrLn $ "    " ++ cDescription command
          putStrLn ""

  putStrLn "Options:"
  forM_ optionMap $ \(name, option) -> do
          putStrLn $ "  " ++ name ++ ": " ++ optionUsage option
  exitWith (ExitFailure 1)

usageSpecific :: Command -> IO a
usageSpecific command = do
  putStrLn $ "Usage: initstore-fs " ++ usageString command
  exitWith (ExitFailure 1)

findCommand :: String -> Maybe Command
findCommand name = listToMaybe [ c | c <- commands, cName c == name ]

envDatabaseType :: String
envDatabaseType = "DBM_DATABASE_TYPE"

envDatabaseName :: String
envDatabaseName = "DBM_DATABASE"

envStoreName :: String
envStoreName = "DBM_MIGRATION_STORE"

main :: IO ()
main = do
  allArgs <- getArgs
  when (null allArgs) usage

  let (commandName:unprocessedArgs) = allArgs
  (opts, args) <- case convertOptions unprocessedArgs of
                    Left e -> putStrLn e >> usage
                    Right c -> return c

  command <- case findCommand commandName of
               Nothing -> usage
               Just c -> return c

  -- Read store path and database connection string from environment
  -- or config file (for now, we just look at the environment).  Also,
  -- require them both to be set for every operation, even though some
  -- operations only require the store path.
  env <- getEnvironment
  let mDbConnStr = lookup envDatabaseName env
      mDbType = lookup envDatabaseType env
      storePathStr = case lookup envStoreName env of
                       Just sp -> sp
                       Nothing -> error $ "Error: missing required environment \
                                          \variable " ++ envStoreName

  let (required,optional) = splitAt (length $ cRequired command) args
      store = FSStore { storePath = storePathStr }

  if (length args) < (length $ cRequired command) then
      usageSpecific command else
      do
        loadedStoreData <- loadMigrations store
        case loadedStoreData of
          Left es -> do
            putStrLn "There were errors in the migration store:"
            forM_ es $ \err -> do
                             putStrLn $ "  " ++ show err
          Right storeData -> do
            let st = AppState { appOptions = opts
                              , appCommand = command
                              , appRequiredArgs = required
                              , appOptionalArgs = optional
                              , appDatabaseConnStr = DbConnDescriptor <$> mDbConnStr
                              , appDatabaseType = mDbType
                              , appStore = FSStore { storePath = storePathStr }
                              , appStoreData = storeData
                              }
            (runReaderT (cHandler command $ storeData) st) `catchSql` reportSqlError
