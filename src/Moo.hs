module Main
    ( main
    )
where

import  Control.Monad (liftM)
import  Data.Configurator
import  Prelude  hiding (lookup)
import  System.Environment (getArgs, getEnvironment)

import  Moo.Core
import  Moo.Main

loadConfiguration :: Maybe FilePath -> IO Configuration
loadConfiguration Nothing = liftM fromShellEnvironment getEnvironment
loadConfiguration (Just path) = fromConfigurator =<< load [Required path]

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  conf <- loadConfiguration $ _configFilePath opts
  mainWithConf args conf
