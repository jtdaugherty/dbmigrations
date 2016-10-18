module Main where
import Prelude
import Test.HUnit
import System.Exit
import System.IO ( stderr )

import qualified DependencyTest
import qualified MigrationsTest
import qualified FilesystemSerializeTest
import qualified FilesystemParseTest
import qualified FilesystemTest
import qualified CycleDetectionTest
import qualified StoreTest
import qualified LinearMigrationsTest
import qualified ConfigurationTest

import Control.Exception ( SomeException(..) )

loadTests :: IO [Test]
loadTests = do

  ioTests <- sequence [ do fspTests <- FilesystemParseTest.tests
                           return $ "Filesystem Parsing" ~: test fspTests
                      , do fsTests <- FilesystemTest.tests
                           return $ "Filesystem general" ~: test fsTests
                      , do linTests <- LinearMigrationsTest.tests
                           return $ "Linear migrations" ~: test linTests
                      , do cfgTests <- ConfigurationTest.tests
                           return $ "Configuration tests" ~: test cfgTests
                      ]
  return $ concat [ ioTests
                  , DependencyTest.tests
                  , FilesystemSerializeTest.tests
                  , MigrationsTest.tests
                  , CycleDetectionTest.tests
                  , StoreTest.tests
                  ]

tempDatabase :: String
tempDatabase = "dbmigrations_test"

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
