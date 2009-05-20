{-# LANGUAGE TemplateHaskell #-}
module Common
    ( TestDependable(..)
    , repoRoot
    , testFile
    )
where

import CommonTH
import System.FilePath ( (</>) )
import Language.Haskell.TH.Syntax (lift)

import Database.Schema.Migrations.Dependencies ( Dependable(..) )

repoRoot :: FilePath
repoRoot = $(getRepoRoot >>= lift)

testFile :: FilePath -> FilePath
testFile fp = repoRoot </> "test" </> fp

instance Dependable TestDependable where
    depId = tdId
    depsOf = tdDeps

data TestDependable = TD { tdId :: String
                         , tdDeps :: [String]
                         }
                      deriving (Show, Eq, Ord)
