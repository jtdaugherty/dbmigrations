module Main
    ( main
    )
where

import  Prelude  hiding (lookup)
import  System.Environment (getArgs)
import  System.Exit

import  Moo.Core
import  Moo.Main

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  conf <- loadConfiguration $ _configFilePath opts
  case conf of
      Left e -> putStrLn e >> exitFailure
      Right c -> mainWithConf args c
