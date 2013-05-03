-- |This module defines the MOO command interface, the commnad line options
-- parser, and helpers to manipulate the Command data structure.
module Moo.CommandInterface
    ( commands
    , commandOptionUsage
    , findCommand
    , getCommandArgs
    , usageString
    ) where

import Data.Maybe
import Moo.CommandHandlers
import Moo.Core
import System.Console.GetOpt


-- |The available commands; used to dispatch from the command line and
-- used to generate usage output.
-- |The available commands; used to dispatch from the command line and
-- used to generate usage output.
commands :: [Command]
commands = [ Command "new" [migrationName]
                           []
                           ["no-ask", configFile]
                           "Create a new empty migration"
                           newCommand

           , Command "apply" [migrationName]
                             []
                             [testOption, configFile]
                             "Apply the specified migration and its \
                             \dependencies"
                             applyCommand

           , Command "revert" [migrationName]
                              []
                              [testOption, configFile]
                              "Revert the specified migration and those \
                              \that depend on it"
                              revertCommand

           , Command "test" [migrationName]
                            []
                            [configFile]
                            "Test the specified migration by applying \
                            \and reverting it in a transaction, then \
                            \roll back"
                            testCommand

           , Command "upgrade" []
                               []
                               [testOption, configFile]
                               "Install all migrations that have not yet \
                               \been installed"

                               upgradeCommand

           , Command "upgrade-list" []
                                    []
                                    []
                                    "Show the list of migrations not yet \
                                    \installed"
                                    upgradeListCommand

           , Command "reinstall" [migrationName]
                                 []
                                 [testOption, configFile]
                                 "Reinstall a migration by reverting, then \
                                 \reapplying it"
                                 reinstallCommand

           , Command "list" []
                            []
                            [configFile]
                            "List migrations already installed in the backend"
                            listCommand
           ]
    where migrationName = "migrationName"
          testOption    = "test"
          configFile    = "config-file"


findCommand :: String -> Maybe Command
findCommand name = listToMaybe [ c | c <- commands, _cName c == name ]


commandOptions :: [ OptDescr (CommandOptions -> IO CommandOptions) ]
commandOptions =  [ optionConfigFile
                  , optionTest
                  , optionNoAsk
                  ]


optionConfigFile :: OptDescr (CommandOptions -> IO CommandOptions)
optionConfigFile = Option "c" ["config-file"]
                   (ReqArg (\arg opt ->
                             return opt { _configFilePath = Just arg }) "FILE")
                   "Specify location of configuration file"


optionTest :: OptDescr (CommandOptions -> IO CommandOptions)
optionTest = Option "t" ["test"]
             (NoArg (\opt -> return opt { _test = True }))
             "Perform the action then rollback when finished"


optionNoAsk :: OptDescr (CommandOptions -> IO CommandOptions)
optionNoAsk = Option "n" ["no-ask"]
              (NoArg (\opt -> return opt { _noAsk = True }))
              "Do not interactively ask any questions, just do it"


getCommandArgs :: [String] -> IO ( CommandOptions, [String] )
getCommandArgs args = do
  let (actions, required, _) =  getOpt RequireOrder commandOptions args
  opts <- foldl (>>=) defaultOptions actions
  return ( opts, required )


defaultOptions :: IO CommandOptions
defaultOptions = return $ CommandOptions Nothing False False


commandOptionUsage :: String
commandOptionUsage = usageInfo "Options:" commandOptions


usageString :: Command -> String
usageString command =
    unwords (_cName command:optionalArgs ++ options ++ requiredArgs)
    where
      requiredArgs = map (\s -> "<" ++ s ++ ">") $ _cRequired command
      optionalArgs = map (\s -> "[" ++ s ++ "]") $ _cOptional command
      options = map (\s -> "["++ "--" ++ s ++ "]")  optionStrings
      optionStrings = _cAllowedOptions command
