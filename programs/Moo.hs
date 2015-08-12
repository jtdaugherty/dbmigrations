module Main
    ( main
    )
where

import  Control.Applicative ((<$>))
import  Data.Configurator
import  Prelude  hiding (lookup)
import  System.Environment (getArgs, getEnvironment)
import  System.Exit

import  Moo.Core
import  Moo.Main

loadConfiguration :: Maybe FilePath -> IO Configuration
loadConfiguration pth = do
    mCfg <- case pth of
        Nothing -> fromShellEnvironment <$> getEnvironment
        Just path -> fromConfigurator =<< load [Required path]

    case mCfg of
        Nothing -> do
            case pth of
                Nothing -> putStrLn "Missing required environment variables"
                Just path -> putStrLn $ "Could not load configuration from " ++ path
            exitFailure
        Just cfg -> return cfg

main :: IO ()
main = do
  args <- getArgs
  (_, opts, _) <- procArgs args
  conf <- loadConfiguration $ _configFilePath opts
  mainWithConf args conf
