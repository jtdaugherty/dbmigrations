
module Main where

import Control.Exception
import Control.Monad.State
import Data.Maybe ( fromJust )
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Database.Schema.Migrations.Migration
    ( MigrationMap
    )
import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Store

title :: String
title = "Migration Manager"

help :: String
help = "q:quit"

data MMState = MMState
    { mmMigrations :: MigrationMap
    , mmStatus :: String
    , mmStyles :: [CursesH.CursesStyle]
    , mmStorePath :: FilePath
    }

type MM = StateT MMState IO

type ToplineWidget = TextWidget
type BotlineWidget = TextWidget
type MsglineWidget = TableWidget
type MigrationListWidget = TableWidget

data MainWidget = MainWidget
    { toplineWidget :: ToplineWidget
    , botlineWidget :: BotlineWidget
    , msglineWidget :: MsglineWidget
    , migrationListWidget :: MigrationListWidget
    }

instance Widget MainWidget where
    draw pos sz hint w = draw pos sz hint (mkRealMainWidget (Just sz) w)
    minSize w = minSize (mkRealMainWidget Nothing w)

data MainEditWidget = MainEditWidget
    { toplineEditWidget :: ToplineWidget
    , botlineEditWidget :: BotlineWidget
    , msglineEditWidget :: MsglineWidget
    }

instance Widget MainEditWidget where
    draw pos sz hint w = draw pos sz hint (mkRealMainEditWidget (Just sz) w)
    minSize w = minSize (mkRealMainEditWidget Nothing w)

runMM :: FilePath -> MigrationMap -> [CursesH.CursesStyle] -> MM a -> IO a
runMM sp migrations cstyles mm =
    evalStateT mm (MMState { mmMigrations = migrations
                           , mmStyles = cstyles
                           , mmStatus = sp ++ " loaded."
                           , mmStorePath = sp
                           })

getSize :: MM (Int, Int)
getSize = liftIO $ Curses.scrSize

styles :: [CursesH.Style]
styles = [ CursesH.defaultStyle
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB
         ]

nthStyle :: Int -> MM CursesH.CursesStyle
nthStyle n =
    do cs <- gets mmStyles
       return $ cs !! n

defStyle :: MM CursesH.CursesStyle
defStyle = nthStyle 0

lineStyle :: MM CursesH.CursesStyle
lineStyle = nthStyle 1

lineDrawingStyle :: MM DrawingStyle
lineDrawingStyle = do
  s <- lineStyle
  return $ mkDrawingStyle s

lineOptions :: MM TextWidgetOptions
lineOptions = do
  sz <- getSize
  ds <- lineDrawingStyle
  return $ TWOptions { twopt_size = TWSizeFixed (1, getWidth sz)
                     , twopt_style = ds
                     , twopt_halign = AlignLeft
                     }

mkToplineWidget :: MM ToplineWidget
mkToplineWidget = do
  opts <- lineOptions
  return $ newTextWidget (opts { twopt_halign = AlignCenter }) title

mkBotlineWidget :: MM BotlineWidget
mkBotlineWidget = do
  opts <- lineOptions
  return $ newTextWidget opts help

-- We need to insert a dummy widget at the lower-right corner of the window,
-- i.e. at the lower-right corner of the message line. Otherwise, an
-- error occurs because drawing a character to this position moves the
-- cursor to the next line, which doesn't exist.
mkMsglineWidget :: MM MsglineWidget
mkMsglineWidget = do
  sz <- getSize
  msg <- gets mmStatus
  let width = getWidth sz
      opts = TWOptions { twopt_size = TWSizeFixed (1, width - 1)
                       , twopt_style = defaultDrawingStyle
                       , twopt_halign = AlignLeft
                       }
      tw = newTextWidget opts msg
      row = [TableCell tw, TableCell $ EmptyWidget (1,1)]
      tabOpts = defaultTBWOptions { tbwopt_minSize = (1, width) }
  return $ newTableWidget tabOpts [row]

nlines :: Int
nlines = 3

migrationListHeight :: (Int, Int) -> Int
migrationListHeight (h, _) = h - nlines

migrationListOptions :: MM TableWidgetOptions
migrationListOptions = do
  sz <- getSize
  return $ TBWOptions { tbwopt_fillCol = Nothing
                      , tbwopt_fillRow = None
                      , tbwopt_activeCols = [0]
                      , tbwopt_minSize = (migrationListHeight sz, getWidth sz)
                      }

mkMigrationListWidget :: MM MigrationListWidget
mkMigrationListWidget = do
  migrationMap <- gets mmMigrations
  sz <- getSize
  let rows = map (migrationRow $ getWidth sz) (Map.keys migrationMap)
  opts <- migrationListOptions
  return $ newTableWidget opts rows
    where migrationRow w s = [TableCell $ newTextWidget
                              (defaultTWOptions { twopt_size = TWSizeFixed (1, w) }) s]

validPos :: Pos -> TableWidget -> Bool
validPos pos w = (getWidth pos) `elem` (tbwopt_activeCols $ tbw_options w) &&
                 (getHeight pos) < (length $ tbw_rows w) &&
                 (getHeight pos) >= 0 &&
                 (getWidth pos) >= 0

moveUp :: TableWidget -> TableWidget
moveUp orig =
    let oldPos = fromJust $ tbw_pos orig
        newPos = ((getHeight $ oldPos) - 1, getWidth oldPos)
    in if validPos newPos orig then orig { tbw_pos = Just newPos } else orig

moveDown :: TableWidget -> TableWidget
moveDown orig =
    let oldPos = fromJust $ tbw_pos orig
        newPos = ((getHeight $ oldPos) + 1, getWidth oldPos)
    in if validPos newPos orig then orig { tbw_pos = Just newPos } else orig

mkMainWidget :: MM MainWidget
mkMainWidget = do
  tlw <- mkToplineWidget
  clw <- mkMigrationListWidget
  blw <- mkBotlineWidget
  msglw <- mkMsglineWidget
  return $ MainWidget tlw blw msglw clw

mkRealMainEditWidget :: (Maybe Size) -> MainEditWidget -> TableWidget
mkRealMainEditWidget msz w =
    let cells = [ TableCell $ toplineEditWidget w
                , TableCell $ botlineEditWidget w
                , TableCell $ msglineEditWidget w ]
        rows = map singletonRow cells
        opts = case msz of
                 Nothing -> defaultTBWOptions
                 Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
        in newTableWidget opts rows

mkRealMainWidget :: Maybe Size -> MainWidget -> TableWidget
mkRealMainWidget msz w =
    let cells = [ TableCell $ toplineWidget w
                , TableCell $ migrationListWidget w
                , TableCell $ botlineWidget w
                , TableCell $ msglineWidget w ]
        rows = map singletonRow cells
        opts = case msz of
                 Nothing -> defaultTBWOptions
                 Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
        in newTableWidget opts rows

updateStateDependentWidgets :: MainWidget -> MM MainWidget
updateStateDependentWidgets w = do
  msgLine <- mkMsglineWidget -- update the message line with the
                             -- state's status
  return $ w { msglineWidget = msgLine }

updateStatus :: String -> MM ()
updateStatus msg = do
  st <- get
  put st { mmStatus = msg }

move :: Direction -> MainWidget -> MM MainWidget
move dir w = do
  let listWidget = case dir of
                     DirUp -> moveUp orig
                     DirDown -> moveDown orig
                     _ -> orig
      orig = migrationListWidget w
  w' <- updateStateDependentWidgets w
  return $ w' { migrationListWidget = listWidget }

resize :: Widget w => MM w -> MM ()
resize f = do
  liftIO $ do Curses.endWin
              Curses.resetParams
              Curses.cursSet Curses.CursorInvisible
              Curses.refresh
  w <- f
  redraw w

redraw :: Widget w => w -> MM ()
redraw w = do
  sz <- getSize
  liftIO $ do draw (0, 0) sz DHNormal w
              Curses.refresh

eventloop :: MainWidget -> MM ()
eventloop w = do
  k <- CursesH.getKey (resize mkMainWidget)
  case k of
    Curses.KeyChar 'q' -> return ()
    Curses.KeyUp       -> process $ move DirUp w
    Curses.KeyDown     -> process $ move DirDown w
    _ -> eventloop w
  where process f = do
                w' <- f
                redraw w'
                eventloop w'

mmMain :: MM ()
mmMain = do
  w <- mkMainWidget
  redraw w
  eventloop w

main :: IO ()
main = do
  args <- getArgs
  let theStorePath = args !! 0
  migrationMap <-
      if length args /= 1
      then do p <- getProgName
              putStrLn ("Usage: " ++ p ++ " <store-path>")
              exitFailure
      else do
        let store = FSStore { storePath = theStorePath }
        loadMigrations store

  runCurses theStorePath migrationMap `finally` CursesH.end
    where runCurses sp migrations = do
            CursesH.start
            cstyles <- CursesH.convertStyles styles
            Curses.cursSet Curses.CursorInvisible
            runMM sp migrations cstyles mmMain
