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
import Data.List ( intercalate, sortBy)
import Control.Monad.Reader ( ReaderT, asks, runReaderT )
import Control.Monad ( when, forM_ )
import Control.Monad.Trans ( liftIO )
import Control.Applicative ( (<$>) )
import Database.HDBC.PostgreSQL ( connectPostgreSQL )
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

data AppState = AppState { appOptions :: [CommandOption]
                         , appCommand :: Command
                         , appRequiredArgs :: [String]
                         , appOptionalArgs :: [String]
                         , appStore :: FilesystemStore
                         , appDatabaseConnStr :: Maybe DbConnDescriptor
                         , appStoreData :: StoreData
                         }

type AppT a = ReaderT AppState IO a

-- The type of actions that are invoked to handle specific commands
type CommandHandler = AppT ()

-- Options which can be used to alter command behavior
data CommandOption = Test
                   | NoAsk
                   deriving (Eq)

data AnyIConnection = forall c. (IConnection c) => AnyIConnection c

data AskDepsChoice = Yes | No | View | Done | Quit
                     deriving (Eq)

type PromptChoices a = [(Char, (a, Maybe String))]

unbufferedGetChar :: IO Char
unbufferedGetChar = do
  bufferingMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin bufferingMode
  return c

mkPromptHelp :: PromptChoices a -> String
mkPromptHelp choices =
    intercalate "" [ [c] ++ ": " ++ (fromJust msg) ++ "\n" |
                     (c, (_, msg)) <- choices, isJust msg ]

hasHelp :: PromptChoices a -> Bool
hasHelp = (> 0) . length . (filter hasMsg)
    where hasMsg (_, (_, m)) = isJust m

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

optionMap :: [(String, CommandOption)]
optionMap = [ ("--test", Test)
            , ("--no-ask", NoAsk)]

optionUsage :: CommandOption -> String
optionUsage Test = "Perform the action in a transaction and issue a \
                   \rollback when finished"
optionUsage NoAsk = "Do not interactively ask any questions, just do it"

hasOption :: CommandOption -> AppT Bool
hasOption o = asks ((o `elem`) . appOptions)

isSupportedCommandOption :: String -> Bool
isSupportedCommandOption s = isJust $ lookup s optionMap

isCommandOption :: String -> Bool
isCommandOption s = take 2 s == "--"

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

makeConnection :: DbConnDescriptor -> IO AnyIConnection
makeConnection (DbConnDescriptor connStr) =
    AnyIConnection <$> connectPostgreSQL connStr

withConnection :: (AnyIConnection -> IO a) -> AppT a
withConnection act = do
  mDbPath <- asks appDatabaseConnStr
  when (isNothing mDbPath) $ error "Error: Database connection string not \
                                   \specified, please set DBM_DATABASE"
  liftIO $ bracket (makeConnection $ fromJust mDbPath)
             (\(AnyIConnection conn) -> disconnect conn) act

interactiveAskDeps :: StoreData -> IO [String]
interactiveAskDeps storeData = do
  -- For each migration in the store, starting with the most recently
  -- added, ask the user if it should be added to a dependency list
  let sorted = sortBy compareTimestamps $ storeMigrations storeData
  interactiveAskDeps' storeData (map mId sorted)
      where
        compareTimestamps m1 m2 = compare (mTimestamp m2) (mTimestamp m1)

askDepsChoices :: PromptChoices AskDepsChoice
askDepsChoices = [ ('y', (Yes, Just "yes, depend on this migration"))
                 , ('n', (No, Just "no, do not depend on this migration"))
                 , ('v', (View, Just "view migration details"))
                 , ('d', (Done, Just "done, do not ask me about more dependencies"))
                 , ('q', (Quit, Just "cancel this operation and quit"))
                 ]

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
newCommand = do
  required <- asks appRequiredArgs
  store <- asks appStore
  storeData <- asks appStoreData
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
upgradeCommand = do
  storeData <- asks appStoreData
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
upgradeListCommand = do
  storeData <- asks appStoreData
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
applyCommand = do
  required <- asks appRequiredArgs
  storeData <- asks appStoreData
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
        ensureBootstrappedBackend conn >> commit conn
        m <- lookupMigration storeData migrationId
        apply m storeData conn
        commit conn
        putStrLn "Successfully applied migrations."

revertCommand :: CommandHandler
revertCommand = do
  required <- asks appRequiredArgs
  storeData <- asks appStoreData
  let [migrationId] = required

  withConnection $ \(AnyIConnection conn) -> do
      ensureBootstrappedBackend conn >> commit conn
      m <- lookupMigration storeData migrationId
      revert m storeData conn
      commit conn
      putStrLn "Successfully reverted migrations."

testCommand :: CommandHandler
testCommand = do
  required <- asks appRequiredArgs
  storeData <- asks appStoreData
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
                              , appStore = FSStore { storePath = storePathStr }
                              , appStoreData = storeData
                              }
            (runReaderT (cHandler command) st) `catchSql` reportSqlError
