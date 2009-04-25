module Main where
import Test.HUnit
import System.Exit
import System.IO ( stderr )

loadTests :: IO [Test]
loadTests = sequence
        [
        ]

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
