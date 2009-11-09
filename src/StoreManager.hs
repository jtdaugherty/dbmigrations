module Main where

import Control.Applicative ( (<$>) )
import Control.Monad.State
import qualified Data.Map as Map
import System.Environment
    ( getArgs
    , getProgName
    , getEnvironment
    )
import System.Exit
    ( exitFailure
    )
import System.IO
    ( Handle
    , hClose
    , openTempFile
    , hPutStr
    )
import System.Directory
    ( getTemporaryDirectory
    )
import System.Process
import System.Posix.Files
    ( removeLink
    )

import Data.Maybe
    ( fromJust
    )

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Migration
    ( Migration(..)
    )
import Database.Schema.Migrations.Store

-- XXX Generalize over all MigrationStore instances
data AppState = AppState { appStoreData :: StoreData
                         , appStore :: FilesystemStore
                         , appMigrationList :: SimpleList
                         , appVty :: Vty
                         }

type AppM = StateT AppState IO

titleAttr :: Attr
titleAttr = def_attr
            `with_back_color` blue
            `with_fore_color` bright_white

bodyAttr :: Attr
bodyAttr = def_attr
           `with_back_color` black
           `with_fore_color` bright_white

fieldAttr :: Attr
fieldAttr = def_attr
            `with_back_color` black
            `with_fore_color` bright_green

selAttr :: Attr
selAttr = def_attr
           `with_back_color` yellow
           `with_fore_color` black

scrollListUp :: AppState -> AppState
scrollListUp appst =
    appst { appMigrationList = scrollUp $ appMigrationList appst }

scrollListDown :: AppState -> AppState
scrollListDown appst =
    appst { appMigrationList = scrollDown $ appMigrationList appst }

eventloop :: (Widget a) => AppM a -> (Event -> AppM Bool) -> AppM ()
eventloop uiBuilder handle = do
  w <- uiBuilder
  vty <- gets appVty
  evt <- liftIO $ do
           (img, _) <- mkImage vty w
           update vty $ pic_for_image img
           next_event vty
  next <- handle evt
  if next then
      eventloop uiBuilder handle else
      return ()

continue :: AppM Bool
continue = return True

stop :: AppM Bool
stop = return False

handleEvent :: Event -> AppM Bool
handleEvent (EvKey KUp []) = modify scrollListUp >> continue
handleEvent (EvKey KDown []) = modify scrollListDown >> continue
handleEvent (EvKey (KASCII 'q') []) = stop
handleEvent (EvKey (KASCII 'e') []) = editCurrentMigration >> continue
handleEvent (EvResize w h) = do
  let wSize = appropriateListWindow $ DisplayRegion (toEnum w) (toEnum h)
  modify (\appst -> appst { appMigrationList = (appMigrationList appst) { scrollWindowSize = wSize }})
  continue
handleEvent _ = continue

withTempFile :: (MonadIO m) => (Handle -> FilePath -> m a) -> m a
withTempFile act = do
  (tempFilePath, newFile) <- liftIO $ createTempFile
  result <- act newFile tempFilePath
  liftIO $ cleanup newFile tempFilePath
  return result
  where
    createTempFile = do
                     tempDir <- getTemporaryDirectory
                     openTempFile tempDir "migration.txt"

    cleanup handle tempFilePath = do
                     (hClose handle) `catch` (\_ -> return ())
                     removeLink tempFilePath

editCurrentMigration :: AppM ()
editCurrentMigration = do
  -- Get the current migration
  m <- gets getSelectedMigration
  store <- gets appStore
  migrationPath <- fullMigrationName store $ mId m
  vty <- gets appVty

  withTempFile $ \tempHandle tempPath ->
      liftIO $ do
        -- Copy the migration to a temporary file
        readFile migrationPath >>= hPutStr tempHandle
        hClose tempHandle

        shutdown vty

        currentEnv <- getEnvironment
        let editor = maybe "vi" id $ lookup "EDITOR" currentEnv

        -- Invoke an editor to edit the temporary file
        (_, _, _, pHandle) <- createProcess $ shell $ editor ++ " " ++ tempPath
        waitForProcess pHandle

        -- Once the editor closes, validate the temporary file
        -- XXX

        -- Replace the original migration with the contents of the
        -- temporary file
        readFile tempPath >>= writeFile migrationPath

  -- Reinitialize application state
  put =<< (liftIO $ mkState store)

getSelectedMigration :: AppState -> Migration
getSelectedMigration appst = fromJust $ Map.lookup (fst $ getSelected list) mMap
    where mMap = storeDataMapping $ appStoreData appst
          list = appMigrationList appst

buildUi :: AppState -> Box
buildUi appst =
    let header = text titleAttr (" " ++ (storePath $ appStore appst) ++ " ")
                 <++> hFill titleAttr '-' 1
                 <++> text titleAttr " Store Manager "
        status = text bodyAttr $ maybe "<no description>" id $ mDesc $ getSelectedMigration appst
        helpBar = text titleAttr "q:quit e:edit "
                  <++> hFill titleAttr '-' 1
    in header
        <--> appMigrationList appst
        <--> helpBar
        <--> status

uiFromState :: AppM Box
uiFromState = buildUi <$> get

readStore :: FilesystemStore -> IO StoreData
readStore store = do
  result <- loadMigrations store
  case result of
    Left es -> do
                putStrLn "There were errors in the migration store:"
                forM_ es $ \err -> do
                    putStrLn $ "  " ++ show err
                exitFailure
    Right theStoreData -> return theStoreData

mkState :: FilesystemStore -> IO AppState
mkState fsStore = do
  vty <- mkVty
  sz <- display_bounds $ terminal vty
  storeData <- readStore fsStore
  let migrationList = mkSimpleList bodyAttr selAttr (appropriateListWindow sz) migrationNames
      migrationNames = Map.keys $ storeDataMapping storeData
  return $ AppState { appStoreData = storeData
                    , appStore = fsStore
                    , appMigrationList = migrationList
                    , appVty = vty
                    }

appropriateListWindow :: DisplayRegion -> Int
appropriateListWindow sz = fromEnum $ region_height sz - 3

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         p <- getProgName
         putStrLn ("Usage: " ++ p ++ " <store-path>")
         exitFailure

  let store = FSStore { storePath = args !! 0 }

  beginState <- mkState store

  -- Capture the new application state because it might contain a new
  -- Vty.
  endState <- execStateT (eventloop uiFromState handleEvent) beginState
  let endVty = appVty endState

  -- Clear the screen.
  reserve_display $ terminal endVty
  shutdown endVty