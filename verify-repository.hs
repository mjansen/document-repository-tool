import System.Process
-- import GHC.Exts
-- import Data.List
import System.FilePath
import Control.Applicative
-- import Control.Monad
-- import System.Directory
-- import System.Posix.Files
import System.Environment
-- import Control.Concurrent.Async

-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
-- import Data.Serialize
-- import GHC.Generics
import Text.Printf

-- import Data.Digest.Pure.SHA

import Common

main :: IO ()
main = do
  [fsroot] <- getArgs  -- pattern is usually "*.pdf" pdf djvu ps tex dvi and .gz versions
  let repoPath = fsroot </> file_repository
  files <- lines <$> readProcess "find" [repoPath, "-type", "f"] ""
  putStrLn "checking ..."
  mapM_ checkFile files
  putStrLn "done"

checkFile :: FilePath -> IO ()
checkFile fPath = do
  let (_, fName) = splitFileName fPath
  if length fName /= 40
    then return ()
    else do
      csum <- sha1sum fPath
      if csum /= fName
        then printf "content of %s has checksum %s, which does not match\n" fPath csum
        else return ()
