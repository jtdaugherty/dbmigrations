module CommonTH
    ( getRepoRoot
    )
where

import Language.Haskell.TH
import System.FilePath ( takeDirectory, combine )
import System.Directory ( getCurrentDirectory, canonicalizePath )

getRepoRoot :: Q FilePath
getRepoRoot =
    do here <- location
       cwd <- runIO getCurrentDirectory
       let thisFileName = combine cwd $ loc_filename here
       -- XXX: This depends on the location of this file in the source tree
       return =<< runIO $ canonicalizePath $ head $ drop 2 $ iterate takeDirectory thisFileName
